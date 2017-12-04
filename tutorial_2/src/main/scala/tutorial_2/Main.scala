package tutorial_2


object Main extends App {

  private val mode = "crossvalidation"
  private val dirInput = "/Users/lukas/git-projects/ms_2017_18/tutorial_2/brown_training"
  private val dirOutput = "/Users/lukas/git-projects/ms_2017_18/tutorial_2/brown_training/output"

  mode match {
    case "train" => Helper.trainMode(dirInput)
    case "annotate" => Helper.annotationMode(dirInput, dirOutput)
    case "crossvalidation" =>
      val now = System.nanoTime
      val precision = Helper.crossValidationMode(dirInput)
      val timeElapsed = (System.nanoTime - now) / 1000000000
      println(s"Precision in 10FoldCrossvalidation was: $precision in $timeElapsed seconds")
  }
}

