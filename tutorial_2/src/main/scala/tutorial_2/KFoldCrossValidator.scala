package tutorial_2

import scala.concurrent.{Await, Future}
import scala.concurrent.duration.Duration
import scala.concurrent.ExecutionContext.Implicits.global

class KFoldCrossValidator(k: Int, files: List[FileWithSentence]) {

  private def roundUp(size: Int) =
    size % k match {
      case 0 => size / k
      case _ => (size - (size % k)) / k + 1
    }

  private def splitAsKFolds(files: List[FileWithSentence]): List[List[FileWithSentence]] =
    files.grouped(roundUp(files.size)).toList

  private def buildTrainData(sets: List[List[FileWithSentence]], i: Int): List[FileWithSentence] =
    sets.zipWithIndex.filter(_._2 != i).foldLeft(List.empty[FileWithSentence]){ (m, x) =>
      m ::: x._1
    }

  private def validateSet(trainSet: List[String], testSet: List[String]): Double = {
    val annotator = new ViterbiAnnotator(new HiddenMarkovModel())
    annotator.fit(trainSet)

    val validationData: List[List[Annotation]] = annotator.validations(testSet)
    val predictionData: List[List[Annotation]] = annotator.annotate(testSet)

    val truePredictions = validationData.zipWithIndex.foldLeft((0, 0)) { (m, x) =>

      val precision = (x._1 zip predictionData(x._2)).foldLeft(m) { (t, s) =>
        if (s._1.posTag.equals(s._2.posTag)) {
          (t._1 + 1, t._2 + 1)
        } else {
          (t._1 + 1, t._2)
        }
      }
      precision
    }
    val precision = truePredictions._2.toDouble / truePredictions._1.toDouble * 100.0
    println(s"${truePredictions._2}  / ${truePredictions._1} => $precision")
    precision
  }

  private def getSentence(sentences: List[FileWithSentence]): List[String] =
    sentences.flatMap(_.sentences)

  private def parValidateSet(sets: List[List[FileWithSentence]], testSet: List[FileWithSentence], i: Int): Double = {
    val trainSet = buildTrainData(sets, i)
    validateSet(getSentence(trainSet), getSentence(testSet))
  }

  def validate(): Double = {
    val sets = splitAsKFolds(files)

    val precisions = sets.zipWithIndex.map { set =>
      Future(parValidateSet(sets, set._1, set._2))
    }

    lazy val results = Await.result(Future.sequence(precisions), Duration.Inf)
    results.sum / sets.size
  }
}

