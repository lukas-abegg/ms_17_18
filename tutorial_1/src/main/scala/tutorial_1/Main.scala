package tutorial_1

import java.io._
import java.util.concurrent.TimeUnit

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.io.{Codec, Source}
import scala.util.matching.Regex
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

  private lazy val console: StringBuilder = new StringBuilder()

  console.append(s"Amount of docs: ${result.docs}\n")
  console.append(s"Amount of words: ${Helper.count(result.words, distinct = false)} (distinct: ${Helper.count(result.words)})\n")

  console.append(s"Amount of topics: ${Helper.count(result.topics, distinct = false)} (distinct: ${Helper.count(result.topics)})\n")
  console.append(s"Amount of places: ${Helper.count(result.places, distinct = false)} (distinct: ${Helper.count(result.places)})\n")
  console.append(s"Amount of people: ${Helper.count(result.people, distinct = false)} (distinct: ${Helper.count(result.people)})\n")

  console.append(s"Most frequent words:\n")

  Helper.getTopNWords(20, result.words).toList foreach {
    case (word, count) => console.append(s"$word $count\n")
  }

  console.append(s"it runs about: ${TimeUnit.MILLISECONDS.convert(System.nanoTime - now, TimeUnit.NANOSECONDS)}\n")

  print(console.toString)

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

  lazy val reutersR: Regex = "((?<=<REUTERS).+?(?=<\\/REUTERS>))+".r

  lazy val topicsR: Regex = "((?<=<TOPICS>).+?(?=<\\/TOPICS>))+".r
  lazy val peopleR: Regex = "((?<=<PEOPLE>).+?(?=<\\/PEOPLE>))+".r
  lazy val placesR: Regex = "((?<=<PLACES>).+?(?=<\\/PLACES>))+".r
  lazy val titleR: Regex = "((?<=<TITLE>).+?(?=<\\/TITLE>))+".r
  lazy val bodyR: Regex = "((?<=<BODY>).+?(?=<\\/BODY>))+".r

  lazy val splitDTag: Regex = "((?<=<D>).+?(?=<\\/D>))+".r

  lazy val splitWhitespace: Regex = "([^\\s]+)".r

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
    source.getLines.toList.addString(content, " ")

    val docs = getDocuments(content.toString)
    val results = Await.result(Future.sequence(docs.map { doc => Future(parseDocument(doc)) }), Duration.Inf)
    Helper.mergeParsedResults(results)
  }

  private def getDocuments(content: String): List[String] =
    Helper.reutersR.findAllIn(content).toList

  private def parseDocument(doc: String): ParsedResult = {
    val f = List(
      Future(Helper.titleR.findFirstIn(doc).map(s => addElements(tokenize(s)))),
      Future(Helper.bodyR.findFirstIn(doc).map(s => addElements(tokenize(s)))),
      Future(Helper.topicsR.findFirstIn(doc).map(s => addElements(parseDTags(s)))),
      Future(Helper.peopleR.findFirstIn(doc).map(s => addElements(parseDTags(s)))),
      Future(Helper.placesR.findFirstIn(doc).map(s => addElements(parseDTags(s))))
    )
    val results = Await.result(Future.sequence(f), Duration.Inf)

    ParsedResult(
      1,
      results.head.getOrElse(Map.empty[String, Int]) |+| results(1).getOrElse(Map.empty[String, Int]),
      results(2).getOrElse(Map.empty[String, Int]),
      results(3).getOrElse(Map.empty[String, Int]),
      results(4).getOrElse(Map.empty[String, Int])
    )
  }

  private def parseDTags(s: String): List[String] =
    Helper.splitDTag.findAllIn(s).toList

  private def addElements(elements: List[String]): Map[String, Int] =
    elements.foldLeft(Map.empty[String, Int]) { (m, x) => m + ((x, m.getOrElse(x, 0) + 1)) }

  private def tokenize(s: String): List[String] =
    Helper.splitWhitespace.findAllIn(s.toLowerCase).toList

}