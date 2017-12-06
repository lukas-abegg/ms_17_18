package tutorial_2


object Main extends App {

  private val mode = args(0)
  private val dirInput = args(1)
  private val dirOutput = if (args.length == 3) args(2) else ""

  val now = System.nanoTime


  mode match {
    case "train" => Helper.trainMode(dirInput)
    case "annotate" => Helper.annotationMode(dirInput, dirOutput)
    case "crossvalidation" =>
      val precision = Helper.crossValidationMode(dirInput)
      println(s"Precision in 10FoldCrossvalidation was: $precision")
    case _ => println(s"Mode $mode is unknown")
  }

  val timeElapsed = (System.nanoTime - now) / 1000000000
  println(s"Job for mode $mode finished in $timeElapsed seconds")
}


