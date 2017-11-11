package tutorial_1

import java.io._
import java.util.concurrent.TimeUnit

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.io.{Codec, Source}
import scalaz.Scalaz._

object Main extends App {

  var now = System.nanoTime

  private lazy val BASE_PATH = "/Users/lukas/git-projects"
  private lazy val REFERENCE_CORPUS = BASE_PATH + "/ms_2017_18/tutorial_1/reference-corpus"
  private lazy val REUTERS_CORPUS = BASE_PATH + "/ms_2017_18/tutorial_1/reuters-corpus"

  private def getListOfXML(dir: File): List[File] =
    dir.listFiles.filter(f => f.isFile && f.getName.endsWith(".xml")).toList

  private def parseXml(xml: File): ParsedResult = {
    val handler = new ReutersHandler(new ParsedResult)
    handler.parse(Source.fromFile(xml)(Codec.UTF8))
  }

  private def getParsedResults: List[Future[ParsedResult]] =
    getListOfXML(new File(REUTERS_CORPUS))
      .map(file => Future(parseXml(file)))

  private def mergeParsedResults(results: List[ParsedResult]): ParsedResult =
    results.reduceLeft { (m, x) =>
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

  val results = Await.result(Future.sequence(getParsedResults), Duration.Inf)
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

  println("it runs about: " + TimeUnit.MILLISECONDS.convert(System.nanoTime - now, TimeUnit.NANOSECONDS))

}

case class ParsedResult(
                         var docs: Int = 0,
                         var words: Map[String, Int] = Map.empty[String, Int],
                         var topics: Map[String, Int] = Map.empty[String, Int],
                         var people: Map[String, Int] = Map.empty[String, Int],
                         var places: Map[String, Int] = Map.empty[String, Int]
                       )

case class PrefixInside(prefix: String) {
  private lazy val r = s"""(.*)$prefix(.*)""".r
  def unapply(x: String): Option[String] = Option(x) collect { case r(_, group) => group }
}

case class Suffix(suffix: String) {
  private lazy val r = s"""(.*)$suffix(.*)""".r
  def unapply(x: String): Option[String] = Option(x) collect { case r(group,_) => group }
}

class ReutersHandler(result: ParsedResult) {

  private lazy val prefREUTERS = PrefixInside("<REUTERS")

  private lazy val prefTOPICS = PrefixInside("<TOPICS>")
  private lazy val suffTOPICS = Suffix("</TOPICS>")
  private lazy val prefPEOPLE = PrefixInside("<PEOPLE>")
  private lazy val suffPEOPLE = Suffix("</PEOPLE>")
  private lazy val prefPLACES = PrefixInside("<PLACES>")
  private lazy val suffPLACES = Suffix("</PLACES>")

  private lazy val prefTITLE = PrefixInside("<TITLE>")
  private lazy val suffTITLE = Suffix("</TITLE>")
  private lazy val prefBODY = PrefixInside("<BODY>")
  private lazy val suffBODY = Suffix("</BODY>")

  private lazy val TITLE = "TITLE"
  private lazy val BODY = "BODY"
  private lazy val TOPICS = "TOPICS"
  private lazy val PLACES = "PLACES"
  private lazy val PEOPLE = "PEOPLE"

  private lazy val xmlTag: StringBuilder = new StringBuilder()

  private def getXmlTag = xmlTag.toString

  def parse(source: io.BufferedSource): ParsedResult = {
    for (line <- source.getLines) {
      line match {
        case prefREUTERS(_) => result.docs += 1
        case prefTOPICS(s) => parseSuffixDContent(TOPICS, s)
        case prefPLACES(s) => parseSuffixDContent(PLACES, s)
        case prefPEOPLE(s) => parseSuffixDContent(PEOPLE, s)
        case prefTITLE(s) => parseSuffixText(TITLE, s)
        case prefBODY(s) => parseSuffixText(BODY, s)
        case s if getXmlTag.length > 0 => getXmlTag match {
          case TITLE | BODY => parseSuffixText(getXmlTag, s)
          case TOPICS | PLACES | PEOPLE => parseSuffixDContent(getXmlTag, s)
          case _ =>
        }
        case _ =>
      }
    }
    result
  }

  private lazy val dContent: StringBuilder = new StringBuilder()

  private def parseSuffixDContent(tag: String, s: String): Unit = {
    dContent.append(" ")
    dContent.append(s)
    dContent.toString match {
      case suffTOPICS(d) => parseDTags(tag, d)
      case suffPLACES(d) => parseDTags(tag, d)
      case suffPEOPLE(d) => parseDTags(tag, d)
      case _ if !getXmlTag.equals(tag) =>
        xmlTag.append(tag)
      case _ =>
    }
  }

  private def parseDTags(tag: String, s: String): Unit = {
    tag match {
      case TOPICS => result.topics = addElements(result.topics, splitElements(s))
      case PLACES => result.places = addElements(result.places, splitElements(s))
      case PEOPLE => result.people = addElements(result.people, splitElements(s))
    }
    xmlTag.setLength(0)
    dContent.setLength(0)
  }

  private val splitDTag = """((?<=<D>).+?(?=<\/D>))+""".r

  private def splitElements(s: String): List[String] =
    splitDTag.findAllIn(s).toList

  private lazy val text: StringBuilder = new StringBuilder()

  private def parseSuffixText(tag: String, s: String): Unit = {
    text.append(" ")
    text.append(s)
    text.toString match {
      case suffTITLE(d) => parseTextContent(d)
      case suffBODY(d) => parseTextContent(d)
      case _ if !getXmlTag.equals(tag) =>
        xmlTag.append(tag)
      case _ =>
    }
  }

  private def parseTextContent(s: String): Unit = {
    result.words = addElements(result.words, tokenize(s))
    xmlTag.setLength(0)
    text.setLength(0)
  }

  private def addElements(list: Map[String, Int], elements: List[String]): Map[String, Int] =
    list |+| elements.foldLeft(Map.empty[String, Int]) { (m, x) => m + ((x, m.getOrElse(x, 0) + 1)) }


  private val splitter = "([^\\s]+)".r

  private def tokenize(s: String) : List[String] =
    splitter.findAllIn(s.toLowerCase).toList

}