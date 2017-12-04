package tutorial_2

class KFoldCrossValidator(k: Int, files: List[FileWithSentence]) {

  private def roundUp(size: Int) =
    (size % k) match {
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

    val truePredictions = validationData.zipWithIndex.foldLeft(0.0) { (m, x) =>

      val precision = (x._1 zip predictionData(x._2)).foldLeft(m) { (t, s) =>
        s._1.posTag.equals(s._2.posTag) match {
          case true => t + 1
          case _ => t
        }
      }
      precision
    }
    truePredictions / validationData.size.toDouble * 100.0
  }

  private def getSentence(sentences: List[FileWithSentence]): List[String] =
    sentences.flatMap(_.sentences)

  def validate(): Double = {
    val sets = splitAsKFolds(files)

    val precision = sets.zipWithIndex.foldLeft(0.0) { (m, x) =>
      val trainSet = buildTrainData(sets, x._2)
      val testSet =  x._1

      val precision = validateSet(getSentence(trainSet), getSentence(testSet))
      m + precision
    }
    precision / sets.size
  }
}

