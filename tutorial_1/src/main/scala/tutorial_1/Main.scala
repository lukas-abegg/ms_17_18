package tutorial_1

import java.io._

import org.xml.sax._
import org.xml.sax.helpers.DefaultHandler
import javax.xml.parsers.SAXParserFactory

import scala.collection.immutable.ListMap
import scala.collection.mutable.ListBuffer
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Success}


object Main extends App {

  def initXMLReader(handler: ContentHandler): XMLReader = {
    val reader = SAXParserFactory.newInstance.newSAXParser.getXMLReader
    reader.setContentHandler(handler)
    reader
  }

  val reader = initXMLReader(new ReutersHandler)

  val REFERENCE_CORPUS = "/Users/lukas/git-projects/ms_2017_18/tutorial_1/reference-corpus"

  def parseXml(xml: File) = {
    reader.parse(new InputSource(new InputStreamReader(new FileInputStream(xml), "UTF-8")))
  }

  def getListOfXML(dir: File) = dir.listFiles.par.filter(f => f.isFile && (f.getName.endsWith(".xml"))).toList

  /*
  val futures: List[Future[Unit]] =
    getListOfXML(new File(REFERENCE_CORPUS)).par.map(file => Future(parseXml(file))).toList
  */

  getListOfXML(new File(REFERENCE_CORPUS)).map(file => parseXml(file))

  /*val f = Future.sequence(futures)
    f.onComplete {
    case Success(s) =>
      println(s.length)
      */

      val handler = reader.getContentHandler.asInstanceOf[ReutersHandler]

      println(s"Anzahl Docs: ${handler.docs}")
      println(s"Anzahl Wörter: ${handler.count(handler.words, distinct = false)}")
      println(s"Anzahl verschiedener Wörter: ${handler.count(handler.words)}")

      println(s"Anzahl Topics: ${handler.count(handler.topics, distinct = false)}")
      println(s"Anzahl verschiedener Topics: ${handler.count(handler.topics)}")
      println(s"Anzahl People: ${handler.count(handler.people, distinct = false)}")
      println(s"Anzahl verschiedener People: ${handler.count(handler.people)}")
      println(s"Anzahl Places: ${handler.count(handler.places, distinct = false)}")
      println(s"Anzahl verschiedener Places: ${handler.count(handler.places)}")

      println(s"Häufigste Wörter:")
      handler.getTopNWords(20, handler.words).toList foreach {
        case (word, count) => println(s"$word $count")
      }
  /*
    case Failure(e) => e.printStackTrace()
  }*/

}

case class Result(docs: Int, words: List[String], topics: List[String], people: List[String], places: List[String])

class ReutersHandler extends DefaultHandler {

  val REUTERS = "REUTERS"
  val TOPICS = "TOPICS"
  val PEOPLE = "PEOPLE"
  val PLACES = "PLACES"
  val TITLE = "TITLE"
  val BODY = "BODY"
  val TEXT = "TEXT"
  val D = "D"

  var docs = 0
  var words = ListBuffer[String]()

  var topics = ListBuffer[String]()
  var people = ListBuffer[String]()
  var places = ListBuffer[String]()


  var tag: String = _
  var tagType: String = _

  override def startElement(uri: String, localName: String, qName: String, attributes: Attributes): Unit =
    qName match {
      case REUTERS => docs += 1
      case TOPICS => tagType = TOPICS
      case PLACES => tagType = PLACES
      case PEOPLE => tagType = PEOPLE
      case TITLE | BODY => tag = TEXT
      case D => tag = D
      case _ =>
    }

  private def addWords(s: String) =
    words ++= tokenize(s)

  private def addElements(list: ListBuffer[String], s: String) =
    list += s

  private def tokenize(s: String) = {
    val regex = "[,.:;'<>\"\\?\\-!\\(\\)\\d]"
    s.toLowerCase.split("[\\s]")
    .par.map(_.trim.toLowerCase)
    .filter(x => !x.matches(regex) && !x.isEmpty).toList
  }

  def getTopNWords(n: Int, xs: Seq[String]): Map[String, Int] =
    ListMap(getWordCounts(xs).toSeq.sortWith(_._2 > _._2): _*).take(n)

  private def getWordCounts(xs: Seq[String]): Map[String, Int] =
    Map(xs.distinct.par.map(x => x -> xs.count(_ == x)).toList: _*)

  def count(xs: Seq[String], distinct: Boolean = true): Int =
    if (distinct) {
      xs.distinct.size
    } else {
      xs.size
    }

  override def characters(ch: Array[Char], start: Int, length: Int): Unit =
    tag match {
      case TEXT => addWords(new String(ch, start, length))
      case D => tagType match {
        case TOPICS => addElements(topics, new String(ch, start, length))
        case PEOPLE => addElements(people, new String(ch, start, length))
        case PLACES => addElements(places, new String(ch, start, length))
        case _ =>
      }
      case _ =>
    }

  override def endElement(uri: String, localName: String, qName: String): Unit =
    tag = null
}