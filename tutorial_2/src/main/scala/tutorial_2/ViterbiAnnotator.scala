package tutorial_2

import tutorial_2.Helper._


case class TableColumn(wordGram: Triple, posGram: List[(Triple, Map[Triple, (String, Double)])])

case class Annotation(word: String, posTag: String)


class ViterbiAnnotator(hmm: HiddenMarkovModel) {

  private def calcProb(e: Double, t: Double) = e + t

  private def getTagProbs(word: Triple, optPrev: Option[TableColumn], laplace: Double):(Triple, (String, Double)) = {
    // each new column
    val list = hmm.getTags().foldLeft(Map.empty[Triple, SimpleMapType]) { (m, tagNew) =>

      // prob for each old
      val probs = hmm.getTags().foldLeft(Map.empty[Double, Triple]) { (m, x) =>
        val prob = optPrev match {
          case Some(prev) => calcProb(hmm.getEmission(word, tagNew), hmm.getTransition(tagNew, x))
          case _ => calcProb(hmm.getEmission(word, tagNew), hmm.getTransition(tagNew, hmm.startTag))
        }
        m + ((x, prob))
      }
      val prob = probs.keys.max
      val prev = probs.getOrElse(probs.keys.max, "")

    }
  }

  def fit(sentences: List[String]) =
    hmm.fit(sentences)

  def annotate(sentences: List[String]) = {
    val laplace = (1.0 * 0.5) / ((hmm.getTags().size * hmm.getTags().size) * 0.5)

    sentences.map { s =>
      val triples = hmm.pred(s)

      // List constructor: List(Word3Gram, Map(POS-Tag3Gram -> (POS-Tag3Gram, Probability)))
      triples.foldLeft(List.empty[TableColumn]) { (m, x) =>
        // Tabelle pro Satz

        val column: TableColumn = m.size match {
          case i if (i == 0) =>
            TableColumn(x, getTagProbs(x, None, laplace))
          case _ => case i if (i > 0) =>
            TableColumn(x, getTagProbs(x, Some(m.last), laplace))
        }
       column :: m
      }
    }
  }
}