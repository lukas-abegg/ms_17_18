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
import scala.util.{Failure, Success, Try}
import scalaz._
import Scalaz._

import scala.io.Source


object Main extends App {

  val BASE_PATH = "/home/lukas/git-projects"
  val REFERENCE_CORPUS = BASE_PATH + "/home/lukas/git-projects"
  val REUTERS_CORPUS = BASE_PATH + "/ms_2017_18/tutorial_1/reuters-corpus"



  def parseXml(xml: File): Unit =
    for (line <- Source.fromFile(xml).getLines) {

    }

  def getListOfXML(dir: File): List[File] =
    dir.listFiles.filter(f => f.isFile && (f.getName.endsWith(".xml"))).toList

  /*
  val futures: List[Future[Unit]] =
    getListOfXML(new File(REFERENCE_CORPUS)).par.map(file => Future(parseXml(file))).toList
  */

  getListOfXML(new File(REUTERS_CORPUS)).map(file => parseXml(file))

  /*val f = Future.sequence(futures)
    f.onComplete {
    case Success(s) =>
      println(s.length)
      */

      val handler = reader.getContentHandler.asInstanceOf[ReutersHandler]

      println(s"Amount of docs: ${handler.docs}")
      println(s"Amount of words: ${handler.count(handler.words, distinct = false)} (distinct: ${handler.count(handler.words)})")

      println(s"Amount of topics: ${handler.count(handler.topics, distinct = false)} (distinct: ${handler.count(handler.topics)})")
      println(s"Amount of places: ${handler.count(handler.places, distinct = false)} (distinct: ${handler.count(handler.places)})")
      println(s"Amount of people: ${handler.count(handler.people, distinct = false)} (distinct: ${handler.count(handler.people)})")

      println(s"Most frequent words:")
      handler.getTopNWords(20, handler.words).toList foreach {
        case (word, count) => println(s"$word $count")
      }
  /*
    case Failure(e) => e.printStackTrace()
  }*/

}

case class ParsedResult(
                         docs: Int = 0,
                         words: Map[String, Int] = Map.empty[String, Int],
                         topics: Map[String, Int] = Map.empty[String, Int],
                         people: Map[String, Int] = Map.empty[String, Int],
                         places: Map[String, Int] = Map.empty[String, Int]
                       )

class ReutersHandler(result: ParsedResult) extends DefaultHandler {

  val REUTERS = "REUTERS"
  val TOPICS = "TOPICS"
  val PEOPLE = "PEOPLE"
  val PLACES = "PLACES"
  val UNKNOWN = "UNKNOWN"
  val TITLE = "TITLE"
  val BODY = "BODY"
  val TEXT = "TEXT"
  val D = "D"

  var tag: String = _
  var tagType: String = _

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

  private def addWords(list: Map[String, Int], s: String): Map[String, Int] =
    list |+| tokenize(s).foldLeft(Map.empty[String, Int]) { (m, x) => m + ((x, m.getOrElse(x, 0) + 1)) }

  private def addElements(list: Map[String, Int], s: String): Map[String, Int] =
    list |+| tokenize(s).foldLeft(Map.empty[String, Int]) { (m, x) => m + ((x, m.getOrElse(x, 0) + 1)) }

  private def tokenize(s: String) =
    //val regex = "[,.:;'<>\"\\?\\-!\\(\\)\\d]"
    s.toLowerCase.split("\\s")
      .filter(!_.isEmpty).toList
    //.par.map(_.trim.toLowerCase)
    //.filter(x => !x.matches(regex) && !x.isEmpty).toList

  def getTopNWords(n: Int, xs: scala.collection.immutable.Map[String, Int]): Seq[(String, Int)] =
    xs.toSeq.sortWith(_._2 > _._2).take(n)

  def count(xs: Map[String, Int], distinct: Boolean = true): Int =
    if (distinct) {
      xs.size
    } else {
      xs.foldLeft(0)(_+_._2)
    }

  override def characters(ch: Array[Char], start: Int, length: Int): Unit =
    tag match {
      case TEXT => words = addWords(words, new String(ch, start, length))
      case D => tagType match {
        case TOPICS => topics = addElements(topics, new String(ch, start, length))
        case PEOPLE => people = addElements(people, new String(ch, start, length))
        case PLACES => places = addElements(places, new String(ch, start, length))
        case _ => ;
      }
      case _ => ;
    }

  override def endElement(uri: String, localName: String, qName: String): Unit =
    tag = null
}