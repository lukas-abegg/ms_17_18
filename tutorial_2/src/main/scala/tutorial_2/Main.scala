package tutorial_1

import java.nio.file.FileSystems
import java.nio.file.Files
import java.nio.charset.StandardCharsets._

import scala.collection.JavaConverters._
import scala.collection.mutable.ListBuffer
import scala.util.matching.Regex
import scalaz.Scalaz._


object Main extends App {

  Helper.parse("/home/lukas/git-projects/ms_2017_18/tutorial_2/brown_training")
}

object Helper {

  private def getListOfFiles(directory: String): List[String] = {
    val dir = FileSystems.getDefault.getPath(directory)
    Files.walk(dir)
      .iterator().asScala
      .filter(Files.isRegularFile(_))
      .toList.map(s => new String(Files.readAllBytes(s), UTF_8))
  }

  def mergeMap(results: List[Map[String, Map[String, String]]]): Map[String, Map[String, String]] =
    results.reduceLeft { (m, x) =>
      m |+| x
    }

  private def addElements(elements: Iterator[List[String]]): Map[String, Map[String, String]] =
    elements.foldLeft(Map.empty[String, Map[String, String]]) { (m, x) =>
      m + ((x.head, m.getOrElse(x.head, Map.empty[String, String]) + (x(1) -> x(2))))
    }

  private def getTriple(r: Regex, lines: List[String]): List[(String, String, String)] =
    lines.flatMap { line =>
      (List("$") ++ r.findAllIn(line).toList ++ List("$")).sliding(3).map { t =>
        (t.head, t(2), t.last)
      }
    }

  def parse(dir: String) = {
    val lines = getListOfFiles(dir).flatMap(splitSentences.findAllIn(_))

    val words = getTriple(getWords, lines)
    val posTags = getTriple(getPOSs, lines)
  }

  lazy val splitSentences: Regex = "[\\t](.+)*".r

  lazy val getWords: Regex = "\\S+(?=\\/\\S+)".r
  lazy val getPOSs: Regex = "(?<=\\S\\/)\\S+".r
}
