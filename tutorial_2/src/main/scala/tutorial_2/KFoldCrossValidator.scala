package tutorial_2


//object KFoldCrossValidator {
//
//  def main(args: Array[String]): Unit = {
//    val validator = new KFoldCrossValidator(10, new Parser("./"))
//
//    val precision = validator.validate(
//      List("The/at Fulton/np County/nn Grand/jj",
//        "The/at Fulton/np County/nn Grand/jj Jury/nn",
//        "The/at Fulton/np County/nn Grand/jj Jury/nn",
//        "The/at Fulton/np County/nn Grand/jj Jury/nn",
//        "The/at Fulton/np County/nn Grand/jj Jury/nn",
//        "The/at Fulton/np County/nn Grand/jj Jury/nn",
//        "The/at Fulton/np County/nn Grand/jj Jury/nn",
//        "The/at Fulton/np County/nn Grand/jj Jury/nn",
//        "The/at Fulton/np County/nn Grand/jj Jury/nn",
//        "The/np Fulton/at County/jj Grand/ss Jury/nn"))
//
//    println("Precision of 10-fold cross-validation was: " + precision + "%")
//  }
//}


class KFoldCrossValidator(k: Int, parser: Parser) { //, annotator: ViterbiAnnotator = null) {

  private def roundUp(size: Int) =
    (size - (size % k)) / k + 1

  private def splitAsKFolds(sentences: List[String]): List[List[String]] =
    sentences.grouped(roundUp(sentences.size)).toList

  private def buildTrainData(sets: List[List[String]], i: Int): List[String] =
    sets.zipWithIndex.filter(_._2 != i).foldLeft(List.empty[String]){ (m, x) =>
      m ::: x._1
    }

  private def validateSet(trainSet: List[String], testSet: List[String]) = {
    //annotator.fit(trainSet);
    //ArrayList<Pair<String, String>> validationData = parser.parse(testSet); // should return List of tuples of words and pos-tags ArrayList<Pair<String,String>>
    val validationData = List[(String, String)](("The", "at"), ("Fulton", "np"))
    //ArrayList<Pair<String, String>> predictionData = annotator.predict(testSet); // should return List of tuples of words and pos-tags ArrayList<Pair<String,String>>
    val predictionData = List[(String, String)](("The", "np"), ("Fulton", "np"))

    val truePredictions = (validationData zip predictionData).foldLeft(0.0){ (m, x) =>
      x._1._2.equals(x._2._2) match {
        case true => m + 1
        case _ => m
      }
    }
    truePredictions / validationData.size * 100
  }

  def validate(sentences: List[String]): Double = {
    val sets = splitAsKFolds(sentences)

    val precision = sets.zipWithIndex.foldLeft(0.0) { (m, x) =>
      val trainSet = buildTrainData(sets, x._2)
      val testSet =  x._1

      m + validateSet(trainSet, testSet)
    }
    precision / sets.size
  }
}

