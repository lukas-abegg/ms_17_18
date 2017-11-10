package tutorial_1

import java.io._
import java.util.concurrent.TimeUnit
import javax.xml.parsers.SAXParserFactory

import org.xml.sax._
import org.xml.sax.helpers.DefaultHandler

import scalaz.Scalaz._

object Main extends App {

  var now = System.nanoTime

  private def initXMLReader(handler: ContentHandler): XMLReader = {
    val reader = SAXParserFactory.newInstance.newSAXParser.getXMLReader
    reader.setContentHandler(handler)
    reader
  }

  private lazy val result = new ParsedResult
  private lazy val reader = initXMLReader(new ReutersHandler(result))

  private lazy val BASE_PATH = "/home/lukas/git-projects"
  private lazy val REFERENCE_CORPUS = BASE_PATH + "/ms_2017_18/tutorial_1/reference-corpus"
  private lazy val REUTERS_CORPUS = BASE_PATH + "/ms_2017_18/tutorial_1/reuters-corpus"

  private def parseXml(xml: File): Unit =
    reader.parse(new InputSource(new InputStreamReader(new FileInputStream(xml), "UTF-8")))

  private def getListOfXML(dir: File): List[File] =
    dir.listFiles.filter(f => f.isFile && f.getName.endsWith(".xml")).toList

  private def getTopNWords(n: Int, xs: scala.collection.immutable.Map[String, Int]): Seq[(String, Int)] =
    xs.toSeq.sortWith(_._2 > _._2).take(n)

  private def count(xs: Map[String, Int], distinct: Boolean = true): Int =
    if (distinct) {
      xs.size
    } else {
      xs.foldLeft(0)(_ + _._2)
    }

  getListOfXML(new File(REUTERS_CORPUS)).foreach(file => parseXml(file))

  println(s"Amount of docs: ${result.docs}")
  println(s"Amount of words: ${count(result.words, distinct = false)} (distinct: ${count(result.words)})")

  println(s"Amount of topics: ${count(result.topics, distinct = false)} (distinct: ${count(result.topics)})")
  println(s"Amount of places: ${count(result.places, distinct = false)} (distinct: ${count(result.places)})")
  println(s"Amount of people: ${count(result.people, distinct = false)} (distinct: ${count(result.people)})")

  println(s"Most frequent words:")
  getTopNWords(20, result.words).toList foreach {
    case (word, count) => println(s"$word $count")
  }

  println("it runs about: "+TimeUnit.MILLISECONDS.convert(System.nanoTime - now, TimeUnit.NANOSECONDS))
}

case class ParsedResult(
                         var docs: Int = 0,
                         var words: Map[String, Int] = Map.empty[String, Int],
                         var topics: Map[String, Int] = Map.empty[String, Int],
                         var people: Map[String, Int] = Map.empty[String, Int],
                         var places: Map[String, Int] = Map.empty[String, Int]
                       )

class ReutersHandler(result: ParsedResult) extends DefaultHandler {

  private lazy val REUTERS = "REUTERS"
  private lazy val TOPICS = "TOPICS"
  private lazy val PEOPLE = "PEOPLE"
  private lazy val PLACES = "PLACES"
  private lazy val UNKNOWN = "UNKNOWN"
  private lazy val TITLE = "TITLE"
  private lazy val BODY = "BODY"
  private lazy val TEXT = "TEXT"
  private lazy val D = "D"

  private var tag: String = _
  private var tagType: String = _

  override def startElement(uri: String, localName: String, qName: String, attributes: Attributes): Unit =
    qName match {
      case REUTERS => result.docs += 1
      case TOPICS => tagType = TOPICS
      case PLACES => tagType = PLACES
      case PEOPLE => tagType = PEOPLE
      case TITLE | BODY => tag = TEXT
      case D => tag = D
      case _ => tagType = UNKNOWN
    }

  private def addElements(list: Map[String, Int], strings: List[String]): Map[String, Int] =
    list |+| strings.foldLeft(Map.empty[String, Int]) { (m, x) => m + ((x, m.getOrElse(x, 0) + 1)) }

  private def tokenize(s: String) =
  //val regex = "[,.:;'<>\"\\?\\-!\\(\\)\\d]"
    s.toLowerCase.split("[\\s+]")
      .filter(!_.isEmpty).toList
  //.par.map(_.trim.toLowerCase)
  //.filter(x => !x.matches(regex) && !x.isEmpty).toList

  private def handlingContent(s: String): Unit =
    tag match {
      case TEXT => result.words = addElements(result.words, tokenize(s))
      case D => tagType match {
        case TOPICS => result.topics = addElements(result.topics, List(s.toLowerCase))
        case PEOPLE => result.people = addElements(result.people, List(s.toLowerCase))
        case PLACES => result.places = addElements(result.places, List(s.toLowerCase))
        case _ => ;
      }
      case _ => ;
    }

  private lazy val sb: StringBuilder = new StringBuilder()

  override def characters(ch: Array[Char], start: Int, length: Int): Unit =
    sb.append(new String(ch, start, length))

  override def endElement(uri: String, localName: String, qName: String): Unit = {
    handlingContent(sb.toString)
    sb.setLength(0)
    tag = null
  }
}