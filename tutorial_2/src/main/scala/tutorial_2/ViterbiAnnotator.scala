package tutorial_2


//                     Word-Unigram /              Pos-Tags /  Prev-Tags / Prob
case class TableColumn(wordGram: Unigram, posGram: Map[Unigram, PrevProb])

case class PrevProb(prev: Unigram, prob: Double)

case class Annotation(word: String, posTag: String)

object Annotation {
  def apply(backTrace: BackTrace): Annotation =
    Annotation(backTrace.word, backTrace.pos)
}

case class BackTrace(word: String, pos: String, prev: Unigram)


class ViterbiAnnotator(hmm: HiddenMarkovModel) {

  private def smoothing(x: Double, laplace: Double) = Math.log(laplace + x)

  private def calcProb(e: Double, t: Double, probPrev: Double) = e + t + probPrev

  private def getTagProbs(word: Unigram, optPrev: Option[TableColumn], laplace: Double): Map[Unigram, PrevProb] = {
    // each new column
    hmm.getTags.foldLeft(Map.empty[Unigram, PrevProb]) { (m, tagNew) =>

      val prevProb = optPrev match {
        case Some(prevTag) =>
          // prob for each old
          val probs = prevTag.posGram.foldLeft(Map.empty[Double, Unigram]) { (m, x) =>
            val e = smoothing(hmm.getEmission(word, tagNew), laplace)
            val t = smoothing(hmm.getTransition(tagNew, x._1), laplace)
            val probPrev = x._2.prob
            val prob: Double = calcProb(e, t, probPrev)
            m + ((prob, x._1))
          }
          val prob: Double = probs.keys.max
          val prev: Unigram = probs.getOrElse(prob, Unigram("", ""))
          PrevProb(prev, prob)
        case _ =>
          val e = smoothing(hmm.getEmission(word, tagNew), laplace)
          val t = smoothing(hmm.getTransition(tagNew, hmm.startTag), laplace)
          val prob: Double = calcProb(e, t, 0.0)
          PrevProb(hmm.startTag, prob)
      }
      m + ((tagNew, prevProb))
    }
  }

  private def startBackTrace(tableColumn: TableColumn): BackTrace = {
    val word = tableColumn.wordGram
    val pos = tableColumn.posGram.reduceLeft { (m, x) =>
      m match {
        case i if i._1.toString.equals("") => x
        case i => i match {
          case j if j._2.prob < x._2.prob => x
          case _ => m
        }
      }
    }
    //                   last tag of unigram
    BackTrace(word.last, pos._1.last, pos._2.prev)
  }

  private def backTrace(tableColumn: TableColumn, last: BackTrace): BackTrace = {
    val wordTriple = tableColumn.wordGram
    val posTriple = last.prev
    val prev = tableColumn.posGram.get(last.prev)

    prev match {
      case Some(prevProb) => BackTrace(wordTriple.last, posTriple.last, prevProb.prev)
      case _ => BackTrace(wordTriple.last, posTriple.last, Unigram(List("", "","")))
    }
  }

  def fit(sentences: List[String]): Unit = hmm.fit(sentences)

  def loadModel(parser: Parser): Unit = hmm.loadModel(parser)

  def saveModel(parser: Parser): Unit = hmm.saveModel(parser)

  def annotate(sentences: List[String]): List[List[Annotation]] = {
    val laplace = (1.0 * 0.5) / ((hmm.getTags.size * hmm.getTags.size) * 0.5)

    val results = sentences.map { s =>
      val triples = hmm.pred(s)

      // List constructor: List(Word3Gram, Map(POS-Tag3Gram -> (POS-Tag3Gram, Probability)))
      val predictions = triples.foldLeft(List.empty[TableColumn]) { (m, x) =>
        // Tabelle pro Satz
        val column: TableColumn = m.size match {
          case i if i == 0 =>
            TableColumn(x, getTagProbs(x, None, laplace))
          case _ =>
            TableColumn(x, getTagProbs(x, Some(m.head), laplace))
        }
        column :: m
      }
      // filter
      val tableColumnsPredicted = predictions.zipWithIndex.foldLeft(List.empty[BackTrace]) { (m, tableColumn) =>
        val result = tableColumn._2 match {
          case 0 => startBackTrace(tableColumn._1)
          case _ => backTrace(tableColumn._1, m.head)
        }
        result :: m
      }
      tableColumnsPredicted.map(Annotation(_))
    }
    results
  }

  def validations(sentences: List[String]): List[List[Annotation]] =
    sentences.map { s =>
      hmm.validations(s)
  }
}