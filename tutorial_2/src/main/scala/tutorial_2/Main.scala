package tutorial_2


object Main extends App {

  val parser = new Parser("/home/lukas/git-projects/ms_2017_18/tutorial_2/brown_training/test")
  val data = parser.preprocess()

  val annotator = new ViterbiAnnotator(new HiddenMarkovModel())
  annotator.fit(data)
  annotator.annotate(data)
}

