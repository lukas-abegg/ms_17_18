package tutorial_1

import java.io._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.io.Source
import scala.util.{Failure, Success}
import scalaz.Scalaz._


object Main extends App {

  private lazy val BASE_PATH = "/home/lukas/git-projects"
  private lazy val REFERENCE_CORPUS = BASE_PATH + "/ms_2017_18/tutorial_1/reference-corpus"
  private lazy val REUTERS_CORPUS = BASE_PATH + "/ms_2017_18/tutorial_1/reuters-corpus"

  private def getListOfXML(dir: File): List[File] =
    dir.listFiles.filter(f => f.isFile && f.getName.endsWith(".xml")).toList

  private def parseXml(xml: File): ParsedResult = {
    val handler = new ReutersHandler(new ParsedResult)
    handler.parse(Source.fromFile(xml))
  }

  private def getParsedResults: List[Future[ParsedResult]] =
    getListOfXML(new File(REFERENCE_CORPUS))
      .map(file => Future(parseXml(file)))

  private def mergeParsedResults(results: List[ParsedResult]): ParsedResult =
    results.reduceLeft { (m, x) =>
      println(ParsedResult(m.docs + x.docs, m.words |+| x.words, m.topics |+| x.topics, m.people |+| x.people, m.places |+| x.places))
      ParsedResult(m.docs + x.docs, m.words |+| x.words, m.topics |+| x.topics, m.people |+| x.people, m.places |+| x.places)
    }

  private def getTopNWords(n: Int, xs: Map[String, Int]): Seq[(String, Int)] =
    xs.toSeq.sortWith(_._2 > _._2).take(n)

  private def count(xs: Map[String, Int], distinct: Boolean = true): Int =
    if (distinct) {
      xs.size
    } else {
      xs.foldLeft(0)(_ + _._2)
    }

  val f = Future.sequence(getParsedResults)
  f.onComplete {
    case Success(results) =>
      val result = mergeParsedResults(results)

      println(s"Amount of docs: ${result.docs}")
      println(s"Amount of words: ${count(result.words, distinct = false)} (distinct: ${count(result.words)})")

      println(s"Amount of topics: ${count(result.topics, distinct = false)} (distinct: ${count(result.topics)})")
      println(s"Amount of places: ${count(result.places, distinct = false)} (distinct: ${count(result.places)})")
      println(s"Amount of people: ${count(result.people, distinct = false)} (distinct: ${count(result.people)})")

      println(s"Most frequent words:")
      getTopNWords(20, result.words).toList foreach {
        case (word, count) => println(s"$word $count")
      }

    case Failure(e) => e.printStackTrace()
  }

}

case class ParsedResult(
                         var docs: Int = 0,
                         var words: Map[String, Int] = Map.empty[String, Int],
                         var topics: Map[String, Int] = Map.empty[String, Int],
                         var people: Map[String, Int] = Map.empty[String, Int],
                         var places: Map[String, Int] = Map.empty[String, Int]
                       )

case class Prefix(prefix: String) {
  def unapply(x: String): Option[String] = if (x startsWith prefix) Some(x.substring(prefix.length)) else None
}

case class PrefixInside(prefix: String) {
  def unapply(x: String): Option[String] = if (x contains prefix) Some(x.substring(x.indexOf(prefix) + prefix.length)) else None
}

case class Suffix(suffix: String) {
  def unapply(x: String): Option[String] = if (x endsWith suffix) Some(x.substring(0, x.length - suffix.length)) else None
}

class ReutersHandler(result: ParsedResult) {

  private lazy val prefREUTERS = Prefix("<REUTERS>")

  private lazy val prefTOPICS = Prefix("<TOPICS>")
  private lazy val suffTOPICS = Suffix("</TOPICS>")
  private lazy val prefPEOPLE = Prefix("<PEOPLE>")
  private lazy val suffPEOPLE = Suffix("</PEOPLE>")
  private lazy val prefPLACES = Prefix("<PLACES>")
  private lazy val suffPLACES = Suffix("</PLACES>")

  private lazy val prefTITLE = PrefixInside("<TITLE>")
  private lazy val suffTITLE = Suffix("</TITLE>")
  private lazy val prefBODY = Prefix("<BODY>")
  private lazy val suffBODY = Suffix("</BODY>")

  private lazy val TITLE = "TITLE"
  private lazy val BODY = "BODY"
  private lazy val TOPICS = "TOPICS"
  private lazy val PLACES = "PLACES"
  private lazy val PEOPLE = "PEOPLE"

  private var xmlTag: String = _

  def parse(source: io.BufferedSource): ParsedResult = {
    for (line <- source.getLines) {
      line match {
        case prefREUTERS(_) => result.docs += 1
        case prefTOPICS(s) => parseSuffixDContent(TOPICS, s)
        case prefPLACES(s) => parseSuffixDContent(PLACES, s)
        case prefPEOPLE(s) => parseSuffixDContent(PEOPLE, s)
        case prefTITLE(s) => parseSuffixText(TITLE, s)
        case prefBODY(s) => parseSuffixText(BODY, s)
        case s if xmlTag != null => xmlTag match {
          case TITLE | BODY => parseSuffixText(xmlTag, s)
          case TOPICS | PLACES | PEOPLE => parseSuffixDContent(xmlTag, s)
        }
        case _ => ;
      }
    }
    result
  }


  var dContent: String = _

  private def parseSuffixDContent(tag: String, s: String): Unit =
    dContent + s match {
      case suffTOPICS(d) => parseDTags(tag, d)
      case suffPLACES(d) => parseDTags(tag, d)
      case suffPEOPLE(d) => parseDTags(tag, d)
      case _ =>
        xmlTag = tag
        dContent += s
    }

  private def parseDTags(tag: String, s: String): Unit = {
    tag match {
      case TOPICS => result.topics = addElements(result.topics, generateElements(s))
      case PLACES => result.places = addElements(result.places, generateElements(s))
      case PEOPLE => result.people = addElements(result.people, generateElements(s))
    }
    xmlTag = null
    dContent = null
  }

  private def generateElements(s: String): List[String] =
    s.split("</D>").map(_.substring("<D>".length)).toList


  private var text: String = _

  private def parseSuffixText(tag: String, s: String): Unit =
    text + s match {
      case suffTITLE(d) => parseTextContent(tag, d)
      case suffBODY(d) => parseTextContent(tag, d)
      case _ =>
        xmlTag = tag
        text += s
    }

  private def parseTextContent(tag: String, s: String): Unit = {
    addElements(result.words, tokenize(s))
    xmlTag = null
    text = null
  }

  private def addElements(list: Map[String, Int], elements: List[String]): Map[String, Int] =
    list |+| elements.foldLeft(Map.empty[String, Int]) { (m, x) => m + ((x, m.getOrElse(x, 0) + 1)) }

  private def tokenize(s: String) =
    s.toLowerCase.split("[\\s+]")
      .filter(!_.isEmpty).toList

}