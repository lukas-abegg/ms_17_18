package tutorial_2

import java.io.{BufferedWriter, File, FileWriter}
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{FileSystems, Files}

import scala.collection.JavaConverters._
import scala.util.matching.Regex


case class FileWithContent(name: String, content: String)

case class FileWithSentence(name: String, sentences: List[String])


class Parser() {

  private def parseFiles(directory: String): List[FileWithContent] = {
    val dir = FileSystems.getDefault.getPath(directory)
    Files.walk(dir)
      .iterator().asScala
      .filter(Files.isRegularFile(_))
      .toList.map { s => FileWithContent(s.getFileName.toString, new String(Files.readAllBytes(s), UTF_8)) }
  }

  private lazy val splitSentences: Regex = "[\\t](.+)*".r

  def parseContent(dir: String): List[FileWithContent] =
    parseFiles(dir)

  def preprocess(dir: String, take10: Boolean = false): List[FileWithSentence] =
    take10 match {
      case true =>
        parseFiles(dir)
          .take(10)
          .map {
            f => FileWithSentence(f.name, splitSentences.findAllIn(f.content).toList)
          }
      case _ =>
        parseFiles(dir)
          .map {
            f => FileWithSentence(f.name, splitSentences.findAllIn(f.content).toList)
          }
    }

  def saveModel(model: Model): Unit = {

  }

  def loadModel(): Model = {
    null
  }

  def writeToFile(dir: String, content: String): Unit = {
    val file = new File(dir)
    val bw = new BufferedWriter(new FileWriter(file))
    bw.write(content)
    bw.close()
  }
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
      .map(key => key -> (map1.getOrElse(key, 0.0) + map2.getOrElse(key, 0.0)))
      .toMap

  def logOf(emissions: NestedMapType): NestedMapType =
    emissions.map { e =>
      e._2 match {
        case ts: Map[Triple, Double] =>
          (e._1, ts.map { t => (t._1, Math.log(t._2 / e._2.values.sum)) })
      }
    }

  def crossValidationMode(dirInput: String): Double = {
    val parser = new Parser()
    val files = parser.preprocess(dirInput, true)

    val kFoldCrossValidator = new KFoldCrossValidator(10, files)
    kFoldCrossValidator.validate()
  }

  def trainMode(dirInput: String): Unit = {
    val parser = new Parser()
    val files = parser.preprocess(dirInput)

    val annotator = new ViterbiAnnotator(new HiddenMarkovModel())
    annotator.fit(files.flatMap(_.sentences))

    parser.saveModel(annotator.getModel)
  }

  private lazy val replacer = "NA"

  private def prepareFileAnnotations(content: String, annotations: List[List[Annotation]]): String =
    annotations.foldLeft(content){ (m, sentence) =>
      val s = sentence.foldLeft(m){ (x, annotation) =>
        x.replaceFirst(replacer, annotation.posTag)
      }
      s
    }

  def annotationMode(dirInput: String, dirOutput: String): Unit = {
    val parser = new Parser()
    val files = parser.preprocess(dirInput)
    val origin = parser.parseContent(dirInput)

    val annotator = new ViterbiAnnotator(new HiddenMarkovModel())
    //annotator.loadModal(data)

    files.zipWithIndex.foreach { f =>
      val annotations = annotator.annotate(f._1.sentences)
      parser.writeToFile(f._1.name, prepareFileAnnotations(origin(f._2).content, annotations))
    }
  }
}