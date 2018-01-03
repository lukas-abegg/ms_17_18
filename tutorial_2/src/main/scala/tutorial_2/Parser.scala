package tutorial_2

import java.io.{FileOutputStream, _}
import java.nio.ByteBuffer
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{FileSystems, Files, Paths}

import boopickle.Default._
import tutorial_2.Helper.NestedMapType

import scala.collection.JavaConverters._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import scala.util.matching.Regex


case class Model(emissions: NestedMapType, transitions: NestedMapType)

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
    if (take10) {
      parseFiles(dir)
        .take(10)
        .map {
          f => FileWithSentence(f.name, splitSentences.findAllIn(f.content).toList)
        }
    } else {
      parseFiles(dir)
        .map {
          f => FileWithSentence(f.name, splitSentences.findAllIn(f.content).toList)
        }
    }

  lazy val modelPath = "./HMM_Model"

  def writeModelToFile(model: Model): Unit = {
    val binModel = Pickle.intoBytes(model)
    writeByteBufferToFile(modelPath, binModel)
  }

  def loadModelFromFile(): Model = {
    val binModel = readByteBufferFromFile(modelPath)
    val model = Unpickle[Model].fromBytes(binModel)
    model
  }

  private def readByteBufferFromFile(dir: String): ByteBuffer = {
    val file = new File(dir)
    val rChannel = new FileInputStream(file).getChannel
    val size = rChannel.size().toInt
    val buffer = ByteBuffer.allocate(size)
    rChannel.read(buffer)
    buffer.flip()
    buffer
    }

  private def writeByteBufferToFile(dir: String, buf: ByteBuffer): Unit = {
    val file = new File(dir)
    val wChannel = new FileOutputStream(file, false).getChannel
    wChannel.write(buf)
    wChannel.close()
  }

  def writeStringToFile(dir: String, content: String): Unit = {
    val path = Paths.get(dir)
    val bw = Files.newBufferedWriter(path)
    bw.write(content)
    bw.close()
  }
}


object Helper {

  type NestedMapType = Map[Unigram, Map[Unigram, Double]]
  type SimpleMapType = Map[Unigram, Double]

  def merge(map1: NestedMapType, map2: NestedMapType): NestedMapType =
    (map1.keySet ++ map2.keySet)
      .map(key => key -> mergeValues(map1.getOrElse(key, Map.empty[Unigram, Double]), map2.getOrElse(key, Map.empty[Unigram, Double])))
      .toMap

  private def mergeValues(map1: SimpleMapType, map2: SimpleMapType): SimpleMapType =
    (map1.keySet ++ map2.keySet)
      .map(key => key -> (map1.getOrElse(key, 0.0) + map2.getOrElse(key, 0.0)))
      .toMap

  def averagedOverAll(emissions: NestedMapType): NestedMapType =
    emissions.map { e =>
      e._2 match {
        case ts: Map[Unigram, Double] =>
          val sum = ts.values.sum
          (e._1, ts.map { t => (t._1 -> t._2 / sum) } )
      }
    }

  def crossValidationMode(dirInput: String): Double = {
    val parser = new Parser()
    val files = parser.preprocess(dirInput, take10 = true)

    val kFoldCrossValidator = new KFoldCrossValidator(10, files)
    kFoldCrossValidator.validate()
  }

  def trainMode(dirInput: String): Unit = {
    val parser = new Parser()
    val files = parser.preprocess(dirInput)

    val annotator = new ViterbiAnnotator(new HiddenMarkovModel())
    annotator.fit(files.flatMap(_.sentences))

    annotator.saveModel(parser)
  }

  private lazy val replacer = "NA"

  private def prepareFileAnnotations(content: String, annotations: List[List[Annotation]]): String =
    annotations.foldLeft(content) { (m, sentence) =>
      val s = sentence.foldLeft(m) { (x, annotation) =>
        x.replaceFirst(replacer, annotation.posTag)
      }
      s
    }

  private def annotateFile(dir: String, f: FileWithSentence, origin: FileWithContent, parser: Parser, annotator: ViterbiAnnotator): String = {
    val annotations = annotator.annotate(f.sentences)
    val filename = dir + f.name
    parser.writeStringToFile(filename, prepareFileAnnotations(origin.content, annotations))
    f.name
  }

  def annotationMode(dirInput: String, dirOutput: String): Unit = {
    val parser = new Parser()
    val files = parser.preprocess(dirInput)
    val origin = parser.parseContent(dirInput)

    val annotator = new ViterbiAnnotator(new HiddenMarkovModel())
    annotator.loadModel(parser)

    val writtenFiles = files.zipWithIndex.map { f =>
      Future(annotateFile(dirOutput, f._1, origin(f._2), parser, annotator))
    }

    lazy val results = Await.result(Future.sequence(writtenFiles), Duration.Inf)
    println(s"${results.size} files have been written to output dir: $dirOutput")
  }
}