package tutorial_1

import java.io._
import java.util.concurrent.TimeUnit

import jdk.nashorn.internal.runtime.ScriptRuntime

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

  private def getParsedResults: List[Future[ParsedResult]] =
    getListOfXML(new File(REFERENCE_CORPUS))
      .map(file => Future(parseXml(file)))

  private def getListOfXML(dir: File): List[File] =
    dir.listFiles.filter(f => f.isFile && f.getName.endsWith(".xml")).toList

  private def parseXml(xml: File): ParsedResult = {
    val handler = new ReutersHandler()
    handler.parse(Source.fromFile(xml)(Codec.UTF8))
  }

  val results = Await.result(Future.sequence(getParsedResults), Duration.Inf)
  val result = Helper.mergeParsedResults(results)

  println(s"Amount of docs: ${result.docs}")
  println(s"Amount of words: ${Helper.count(result.words, distinct = false)} (distinct: ${Helper.count(result.words)})")

  println(s"Amount of topics: ${Helper.count(result.topics, distinct = false)} (distinct: ${Helper.count(result.topics)})")
  println(s"Amount of places: ${Helper.count(result.places, distinct = false)} (distinct: ${Helper.count(result.places)})")
  println(s"Amount of people: ${Helper.count(result.people, distinct = false)} (distinct: ${Helper.count(result.people)})")

  println(s"Most frequent words:")
  Helper.getTopNWords(20, result.words).toList foreach {
    case (word, count) => println(s"$word $count")
  }

  println("it runs about: " + TimeUnit.MILLISECONDS.convert(System.nanoTime - now, TimeUnit.NANOSECONDS))

}


object Helper {

  def mergeParsedResults(results: List[ParsedResult]): ParsedResult =
    results.reduceLeft { (m, x) =>
      ParsedResult(m.docs + x.docs, m.words |+| x.words, m.topics |+| x.topics, m.people |+| x.people, m.places |+| x.places)
    }

  def getTopNWords(n: Int, xs: Map[String, Int]): Seq[(String, Int)] =
    xs.toSeq.sortWith(_._2 > _._2).take(n)

  def count(xs: Map[String, Int], distinct: Boolean = true): Int =
    if (distinct) {
      xs.size
    } else {
      xs.foldLeft(0)(_ + _._2)
    }
}


case class ParsedResult(
                         var docs: Int = 0,
                         var words: Map[String, Int] = Map.empty[String, Int],
                         var topics: Map[String, Int] = Map.empty[String, Int],
                         var people: Map[String, Int] = Map.empty[String, Int],
                         var places: Map[String, Int] = Map.empty[String, Int]
                       )


class ReutersHandler() {

  private val content: StringBuilder = new StringBuilder()

  def parse(source: io.BufferedSource): ParsedResult = {
    for (line <- source.getLines()) {
      content.append(" " + line.stripLineEnd)
    }

    val docs = getDocuments(content.toString())
    val results = Await.result(Future.sequence(docs.map { doc => Future(parseDocument(doc)) }), Duration.Inf)
    Helper.mergeParsedResults(results)
  }

  private lazy val reuters = "((?<=<REUTERS).+?(?=<\\/REUTERS>))+".r

  private def getDocuments(content: String): List[String] =
    reuters.findAllIn(content).toList

  private lazy val topicsR = "((?<=<TOPICS>).+?(?=<\\/TOPICS>))+".r
  private lazy val peopleR = "((?<=<PEOPLE>).+?(?=<\\/PEOPLE>))+".r
  private lazy val placesR = "((?<=<PLACES>).+?(?=<\\/PLACES>))+".r
  private lazy val titleR = "((?<=<TITLE>).+?(?=<\\/TITLE>))+".r
  private lazy val bodyR = "((?<=<BODY>).+?(?=<\\/BODY>))+".r


  private def parseDocument(doc: String): ParsedResult = {
    val f = List(
      Future(topicsR.findFirstIn(doc).map(s => addElements(parseDTags(s)))),
      Future(placesR.findFirstIn(doc).map(s => addElements(parseDTags(s)))),
      Future(peopleR.findFirstIn(doc).map(s => addElements(parseDTags(s)))),
      Future(titleR.findFirstIn(doc).flatMap(title => bodyR.findFirstIn(doc).map(body => addElements(tokenize(title + "" + body)))))
    )
    val results = Await.result(Future.sequence(f), Duration.Inf)

    ParsedResult(
      1,
      results(3).getOrElse(Map.empty[String, Int]),
      results(0).getOrElse(Map.empty[String, Int]),
      results(1).getOrElse(Map.empty[String, Int]),
      results(2).getOrElse(Map.empty[String, Int])
    )
  }

  private val splitDTag = "((?<=<D>).+?(?=<\\/D>))+".r

  private def parseDTags(s: String): List[String] =
    splitDTag.findAllIn(s).toList

  private def addElements(elements: List[String]): Map[String, Int] =
    elements.foldLeft(Map.empty[String, Int]) { (m, x) => m + ((x, m.getOrElse(x, 0) + 1)) }


  private lazy val splitter = "([^\\s]+)".r

  private def tokenize(s: String): List[String] =
    splitter.findAllIn(s.toLowerCase).toList

}