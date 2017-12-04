package tutorial_2


//                     Word-Triple /              Prev-Tags /  Pos-Tags /  Prob
case class TableColumn(wordGram: Triple, posGram: Map[Triple, (Triple, Double)])

case class Annotation(word: String, posTag: String)

object Annotation {
  def apply(backTrace: BackTrace): Annotation =
    Annotation(backTrace.word, backTrace.pos)
}

case class BackTrace(word: String, pos: String, prev: Triple)


class ViterbiAnnotator(hmm: HiddenMarkovModel) {

  private def smoothing(x: Double, laplace: Double) = laplace + x

  private def calcProb(e: Double, t: Double, probPrev: Double) = e + t + probPrev

  private def getTagProbs(word: Triple, optPrev: Option[TableColumn], laplace: Double): Map[Triple, (Triple, Double)] = {
    // each new column
    hmm.getTags().foldLeft(Map.empty[Triple, (Triple, Double)]) { (m, tagNew) =>

      val prob = optPrev match {
        case Some(prevTag) =>
          // prob for each old
          val probs = prevTag.posGram.foldLeft(Map.empty[Double, Triple]) { (m, x) =>
            val e = smoothing(hmm.getEmission(word, tagNew), laplace)
            val t = smoothing(hmm.getTransition(tagNew, x._2._1), laplace)
            val probPrev = x._2._2
            val prob: Double = calcProb(e, t, probPrev)
            m + ((prob, x._2._1))
          }
          val prob: Double = probs.keys.max
          val prev: Triple = probs.getOrElse(prob, Triple("", ""))
          (prev, prob)
        case _ =>
          val e = smoothing(hmm.getEmission(word, tagNew), laplace)
          val t = smoothing(hmm.getTransition(tagNew, hmm.startTag), laplace)
          val prob: Double = calcProb(e, t, 0.0)
          (hmm.startTag, prob)
      }
      m + ((prob._1, (tagNew, prob._2)))
    }
  }

  private def startBackTrace(tableColumn: TableColumn): BackTrace = {
    val word = tableColumn.wordGram
    val pos = tableColumn.posGram.reduceLeft { (m, x) =>
      m match {
        case i if i._1.equals("") => x
        case i => i match {
          case j if j._2._2 < x._2._2 => x
          case _ => m
        }
      }
    }
    BackTrace(word.last, pos._2._1.last, pos._1)
  }

  private def backTrace(tableColumn: TableColumn, last: BackTrace): BackTrace = {
    val wordTriple = tableColumn.wordGram
    val posTriple = last.prev
    val prevTriple = tableColumn.posGram.getOrElse(last.prev, (Triple("", ""), 0.0))._1

    BackTrace(wordTriple.last, posTriple.last, prevTriple)
  }

  def fit(sentences: List[String]) =
    hmm.fit(sentences)

  def getModel = hmm.getModel

  def annotate(sentences: List[String]): List[List[Annotation]] = {
    val laplace = (1.0 * 0.5) / ((hmm.getTags().size * hmm.getTags().size) * 0.5)

    val results = sentences.map { s =>
      val triples = hmm.pred(s)

      // List constructor: List(Word3Gram, Map(POS-Tag3Gram -> (POS-Tag3Gram, Probability)))
      val predictions = triples.foldLeft(List.empty[TableColumn]) { (m, x) =>
        // Tabelle pro Satz
        val column: TableColumn = m.size match {
          case i if (i == 0) =>
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
      tableColumnsPredicted.take(tableColumnsPredicted.size-2).map(Annotation(_))
    }
    results
  }

  def validations(sentences: List[String]): List[List[Annotation]] =
    sentences.map { s =>
      hmm.validations(s)
  }
}