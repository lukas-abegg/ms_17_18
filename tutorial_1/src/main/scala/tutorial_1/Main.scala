package tutorial_1

import java.io.{File, StringReader}

import org.xml.sax._
import org.xml.sax.helpers.DefaultHandler
import javax.xml.parsers.SAXParserFactory

import scala.collection.immutable.ListMap
import scala.collection.mutable.ListBuffer

object Main extends App {

  def initXMLReader(handler: ContentHandler): XMLReader = {
    val reader = SAXParserFactory.newInstance.newSAXParser.getXMLReader
    reader.setContentHandler(handler)
    reader
  }

  val reader = initXMLReader(new ReutersHandler)

  val xml = new InputSource(new StringReader("<?xml version=\"1.0\"?>\n<company>\n\t<staff>\n\t\t<firstname>yong</firstname>\n\t\t<lastname>mook kim</lastname>\n\t\t<nickname>mkyong</nickname>\n\t\t<salary>100000</salary>\n\t</staff>\n\t<staff>\n\t\t<firstname>low</firstname>\n\t\t<lastname>yin fong</lastname>\n\t\t<nickname>fong fong</nickname>\n\t\t<salary>200000</salary>\n\t</staff>\n</company>"))

  reader.parse(xml)

  val handler = reader.getContentHandler.asInstanceOf[ReutersHandler]

  println(s"Anzahl Docs: ${handler.docs}")
  println(s"Anzahl Words: ${handler.count(handler.words, false)}")
  println(s"Anzahl verschiedener Wörter: ${handler.count(handler.words)}")

  println(s"Anzahl Topics: ${handler.count(handler.topics, false)}")
  println(s"Anzahl verschiedener Topics: ${handler.count(handler.topics)}")
  println(s"Anzahl People: ${handler.count(handler.people, false)}")
  println(s"Anzahl verschiedener People: ${handler.count(handler.people)}")
  println(s"Anzahl Places: ${handler.count(handler.places, false)}")
  println(s"Anzahl verschiedener Places: ${handler.count(handler.places)}")

  println(s"Häufigste Wörter:")
  handler.getTopNWords(30, handler.words).toList foreach { case (word, count) =>
    println(s"${word} ${count}")
  }
}

class ReutersHandler extends DefaultHandler {

  var docs = 0
  var words = ListBuffer[String]()

  var topics = ListBuffer[String]()
  var people = ListBuffer[String]()
  var places = ListBuffer[String]()

  var tag: String = null
  var tagType: String = null

  override def startElement(uri: String, localName: String, qName: String, attributes: Attributes) =
    qName match {
      case "REUTERS" => docs += 1
      case "TOPICS" => tagType = "topics"
      case "PEOPLE" => tagType = "people"
      case "PLACES" => tagType = "places"
      case "TITLE" | "BODY" => tag = "text"
      case "D" => tag = "d"
      case _ => println("lewi")
    }

  private def addWords(s: String) =
    words ++= tokenize(s)

  private def addElements(list: ListBuffer[String], s: String) =
    list += s

  private def tokenize(s: String) = {
    val regex = "[,.:;'\"\\?\\-!\\(\\)]".r
    s.toLowerCase.split("[\\s]")
      .map(word => regex.replaceAllIn(word.trim.toLowerCase, ""))
      .filter(word => !word.isEmpty)
  }

  def getTopNWords(n: Int, xs: Seq[String]): Map[String, Int] =
    ListMap(getWordCounts(xs).toSeq.sortWith(_._2 > _._2): _*).take(n)

  private def getWordCounts(xs: Seq[String]): Map[String, Int] =
    Map(xs.distinct.map(x => x -> xs.count(_ == x)): _*)

  def count(xs: Seq[String], distinct: Boolean = true): Int =
    distinct match {
      case true => xs.distinct.size
      case _ => xs.size
    }

  override def characters(ch: Array[Char], start: Int, length: Int) =
    tag match {
      case "text" => addWords(new String(ch, start, length))
      case "d" => tagType match {
        case "topics" => addElements(topics, new String(ch, start, length))
        case "people" => addElements(people, new String(ch, start, length))
        case _ => addElements(places, new String(ch, start, length))
      }
      case _ => println("element unknown")
    }

  override def endElement(uri: String, localName: String, qName: String) =
    tag = null
}