package tutorial_1

import java.io._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.io.{Codec, Source}
import scala.util.Sorting
import scala.util.matching.Regex
import scalaz.Scalaz._

object Main extends App {

  lazy val results = Await.result(Future.sequence(Helper.getParsedResults(args(0))), Duration.Inf)
  Helper.printResults(Helper.mergeParsedResults(results))
}


object Helper {

  private lazy val console: StringBuilder = new StringBuilder()

  def printResults(result: ParsedResult): Unit = {
    console.append(s"Anzahl Dokumente: ${result.docs}\n")
    console.append(s"Anzahl Wörter: ${Helper.count(result.words, distinct = false)} (${Helper.count(result.words)} distinct)\n")
    console.append(s"Anzahl Topics: ${Helper.count(result.topics, distinct = false)} (${Helper.count(result.topics)} distinct)\n")
    console.append(s"Anzahl Places: ${Helper.count(result.places, distinct = false)} (${Helper.count(result.places)} distinct)\n")
    console.append(s"Anzahl People: ${Helper.count(result.people, distinct = false)} (${Helper.count(result.people)} distinct)\n")

    console.append(s"Häufigste Wörter:\n")
    Helper.getTopNWords(30, result.words.toArray).toList foreach {
      case (word, count) => console.append(s"$word $count\n")
    }

    print(console.toString)
  }

  def getParsedResults(path: String): List[Future[ParsedResult]] =
    getListOfXML(new File(path))
        .map(file => Future(parseXml(file)))

  private def getListOfXML(dir: File): List[File] =
    dir.listFiles.filter(f => f.isFile && f.getName.endsWith(".xml")).toList

  private def parseXml(xml: File): ParsedResult = {
    lazy val handler = new ReutersHandler()
    handler.parse(Source.fromFile(xml)(Codec.UTF8))
  }

  def mergeParsedResults(results: List[ParsedResult]): ParsedResult =
    results.reduceLeft { (m, x) =>
      ParsedResult(m.docs + x.docs, m.words |+| x.words, m.topics |+| x.topics, m.people |+| x.people, m.places |+| x.places)
    }

  def getTopNWords(n: Int, xs: Array[(String, Int)]): Array[(String, Int)] = {
    Sorting.quickSort(xs)(Ordering.by((_: (String, Int))._2).reverse)
    xs.take(n)
  }

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

  private lazy val content: StringBuilder = new StringBuilder()

  def parse(source: io.BufferedSource): ParsedResult = {
    source.getLines.toList.addString(content, " ")

    lazy val results = getDocuments(content.toString).map { doc => parseDocument(doc) }
    Helper.mergeParsedResults(results)
  }

  private def getDocuments(content: String): List[String] =
    Helper.reutersR.findAllIn(content).toList

  private def parseDocument(doc: String): ParsedResult =
    ParsedResult(
      1,
      Helper.titleR.findFirstIn(doc).map(s => addElements(tokenize(s))).getOrElse(Map.empty[String, Int]) |+|
        Helper.bodyR.findFirstIn(doc).map(s => addElements(tokenize(s))).getOrElse(Map.empty[String, Int]),
      Helper.topicsR.findFirstIn(doc).map(s => addElements(parseDTags(s))).getOrElse(Map.empty[String, Int]),
      Helper.peopleR.findFirstIn(doc).map(s => addElements(parseDTags(s))).getOrElse(Map.empty[String, Int]),
      Helper.placesR.findFirstIn(doc).map(s => addElements(parseDTags(s))).getOrElse(Map.empty[String, Int])
    )

  private def parseDTags(s: String): List[String] =
    Helper.splitDTag.findAllIn(s).toList

  private def addElements(elements: List[String]): Map[String, Int] =
    elements.foldLeft(Map.empty[String, Int]) { (m, x) => m + ((x, m.getOrElse(x, 0) + 1)) }

  private def tokenize(s: String): List[String] =
    Helper.splitWhitespace.findAllIn(s.toLowerCase).toList
}