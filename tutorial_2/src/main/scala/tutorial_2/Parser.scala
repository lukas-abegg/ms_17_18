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


object Helper {

  type NestedMapType = Map[Triple, Map[Triple, Double]]
  type SimpleMapType = Map[Triple, Double]

  def merge(map1: NestedMapType, map2: NestedMapType): NestedMapType =
    (map1.keySet ++ map2.keySet)
      .map(key => key -> mergeValues(map1.getOrElse(key, Map.empty[Triple, Double]), map2.getOrElse(key, Map.empty[Triple, Double])))
      .toMap

  private def mergeValues(map1: SimpleMapType, map2: SimpleMapType): SimpleMapType =
    (map1.keySet ++ map2.keySet)
      .map(key => key -> (map1.getOrElse(key, 0) + map2.getOrElse(key, 0)))
      .toMap

  def logOf(emissions: NestedMapType): NestedMapType =
    emissions.map { e =>
      e._2 match {
        case ts: Map[Triple, Double] =>
          (e._1, ts.map { t => (t._1, Math.log(t._2 / e._2.values.sum)) })
      }
    }
}