package tutorial_2

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{FileSystems, Files}

import scala.collection.JavaConverters._

import scala.util.matching.Regex


class Parser(dir: String) {

  private def parseFiles(directory: String): List[String] = {
    val dir = FileSystems.getDefault.getPath(directory)
    Files.walk(dir)
      .iterator().asScala
      .filter(Files.isRegularFile(_))
      .toList.map(s => new String(Files.readAllBytes(s), UTF_8))
  }

  private lazy val splitSentences: Regex = "[\\t](.+)*".r

  def preprocess(): List[String] =
    parseFiles(dir)
      .take(1)
      .flatMap(splitSentences.findAllIn)
}