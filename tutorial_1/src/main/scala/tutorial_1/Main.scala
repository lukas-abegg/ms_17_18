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

  private lazy val BASE_PATH = "/home/lukas/git-projects"
  private lazy val REFERENCE_CORPUS = BASE_PATH + "/ms_2017_18/tutorial_1/reference-corpus"
  private lazy val REUTERS_CORPUS = BASE_PATH + "/ms_2017_18/tutorial_1/reuters-corpus"

  private def getListOfXML(dir: File): List[File] =
    dir.listFiles.filter(f => f.isFile && f.getName.endsWith(".xml")).toList

  private def parseXml(xml: File): ParsedResult = {
    val handler = new ReutersHandler(new ParsedResult)
    handler.parse(Source.fromFile(xml)(Codec.UTF8))
  }

  private def getParsedResults: List[Future[ParsedResult]] =
    getListOfXML(new File(REFERENCE_CORPUS))
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

  val f = Future.sequence(getParsedResults)
  f.map { results =>
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

  Await.result(f, 10 seconds)

}

case class ParsedResult(
                         var docs: Int = 0,
                         var words: Map[String, Int] = Map.empty[String, Int],
                         var topics: Map[String, Int] = Map.empty[String, Int],
                         var people: Map[String, Int] = Map.empty[String, Int],
                         var places: Map[String, Int] = Map.empty[String, Int]
                       )

case class Prefix(prefix: String) {
  private lazy val r = s"""^$prefix(.*)""".r
  def unapply(x: String): Option[String] = Option(x) collect { case r(group) => group }
}

case class PrefixInside(prefix: String) {
  private lazy val r = s"""(.*)$prefix(.*)""".r
  def unapply(x: String): Option[String] = Option(x) collect { case r(prefix, group) => group }
}

case class Suffix(suffix: String) {
  private lazy val r = s"""(.*)$suffix""".r
  def unapply(x: String): Option[String] = Option(x) collect { case r(group) => group }
}

class ReutersHandler(result: ParsedResult) {

  private lazy val prefREUTERS = Prefix("<REUTERS")

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
        case s if getXmlTag != null => getXmlTag match {
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

  private def parseSuffixDContent(tag: String, s: String): Unit =
    dContent.append(s).toString match {
      case suffTOPICS(d) => parseDTags(tag, d)
      case suffPLACES(d) => parseDTags(tag, d)
      case suffPEOPLE(d) => parseDTags(tag, d)
      case _ =>
        xmlTag.append(tag)
        dContent.append(s)
    }

  private def parseDTags(tag: String, s: String): Unit =
    tag match {
      case TOPICS => result.topics = addElements(result.topics, generateElements(s))
      case PLACES => result.places = addElements(result.places, generateElements(s))
      case PEOPLE => result.people = addElements(result.people, generateElements(s))
    }

  xmlTag.setLength(0)
  dContent.setLength(0)

  private val splitD_END = """<D>(.*)</D>""".r

  private def generateElements(s: String): List[String] =
    splitD_END.findAllIn(s).toList

  private lazy val text: StringBuilder = new StringBuilder()

  private def parseSuffixText(tag: String, s: String): Unit =
    text + s match {
      case suffTITLE(d) => parseTextContent(tag, d)
      case suffBODY(d) => parseTextContent(tag, d)
      case _ =>
        xmlTag.append(tag)
        text.append(s)
    }

  private def parseTextContent(tag: String, s: String): Unit =
    addElements(result.words, tokenize(s))

  xmlTag.setLength(0)
  text.setLength(0)

  private def addElements(list: Map[String, Int], elements: List[String]): Map[String, Int] =
    list |+| elements.foldLeft(Map.empty[String, Int]) { (m, x) => m + ((x, m.getOrElse(x, 0) + 1)) }

  private val splitter = "[\\s+]".r

  private def tokenize(s: String) =
    splitter.findAllIn(s.toLowerCase).filter(!_.isEmpty).toList
}