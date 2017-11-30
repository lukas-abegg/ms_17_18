package tutorial_1

import java.nio.file.FileSystems
import java.nio.file.Files
import java.nio.charset.StandardCharsets._

import scala.collection.JavaConverters._
import scala.util.matching.Regex
import scalaz.Scalaz._


object Main extends App {

  PosTagger.fit("/Users/lukas/git-projects/ms_2017_18/tutorial_2/brown_training/test")
}

case class Trigram(pos1: String, pos2: String, pos3: String)

object PosTagger {

  private var trainedWords: Map[Trigram, Int] = null
  private var trainedPOSTags: Map[Trigram, Int] = null

  private var predWords: Map[Trigram, Int] = null

  private def getListOfFiles(directory: String): List[String] = {
    val dir = FileSystems.getDefault.getPath(directory)
    Files.walk(dir)
      .iterator().asScala
      .filter(Files.isRegularFile(_))
      .toList.map(s => new String(Files.readAllBytes(s), UTF_8))
  }

  private def buildTriple(elements: Iterator[List[String]]) =
    elements.foldLeft(Map.empty[Trigram, Int]) { (m, x) =>
      val trigram = Trigram(x.head, x(1), x.last)
      m + ((trigram, m.getOrElse(trigram, 0) + 1))
    }

  private def getTriple(r: Regex, lines: List[String]): Map[Trigram, Int] =
    lines.foldLeft(Map.empty[Trigram, Int]) { (m, line) =>
      m |+| buildTriple(tokenizeLine(r, line).sliding(3))
    }

  private def tokenizeLine(r: Regex, line: String): List[String]  =
    (List("$") ++ r.findAllIn(line).toList ++ List("$"))

  private lazy val splitSentences: Regex = "[\\t](.+)*".r
  private lazy val getWords: Regex = "\\S+(?=\\/\\S+)".r
  private lazy val getPOSs: Regex = "(?<=\\S\\/)\\S+".r

  def fit(dir: String) = {
    val lines = getListOfFiles(dir).take(10).flatMap(splitSentences.findAllIn(_))
    this.trainedWords = getTriple(getWords, lines)
    this.trainedPOSTags = getTriple(getPOSs, lines)
  }

  def pred(dir: String) = {
    val lines = getListOfFiles(dir).take(10).flatMap(splitSentences.findAllIn(_))
    this.predWords = getTriple(getWords, lines)
  }
}
