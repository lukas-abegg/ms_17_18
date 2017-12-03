package tutorial_2


case class TableColumn(wordGram: Triple, posGram: Map[Triple, Map[Triple, Double]])

case class Annotation(word: String, posTag: String)


class ViterbiAnnotator(hmm: HiddenMarkovModel) {

  private def getTagProbs(w: Triple): Map[Triple,  Map[Triple, Double]] = {
    hmm.getPOSTags().foldLeft(Map.empty[Triple,  Map[Triple, Double]]) { (m, x) =>
      x
      m
      null
    }
  }

  private def probStart():Double = null

  private def probInside():Double = null

  private def calcProbs(w: Triple, prevs: Option[Map[Triple, Double]]) = {
    prevs match {
      // Triple wird im vorherigen mit _3 = dieses Zeichen berechnet
      case Some(previous) => previous.map { case (k, v) => getTagProbs(w, k.prev).valuesIterator.max }
      case _ => getTagProbs(w, w.prev).valuesIterator.max
    }
  }

  def fit(sentences: List[String]) =
    hmm.fit(sentences)

  def annotate(sentences: List[String]) = {
    sentences.map { s =>
      val triples = hmm.pred(s)

      // List constructor: List(Word3Gram -> Map(POS-Tag3Gram -> Map(POS-Tag3Gram -> Probability)))
      triples.foldLeft(List.empty[TableColumn]) { (m, triple) =>
        // Tabelle pro Satz
        TableColumn(triple, getTagProbs(triple)) :: m
      }
    }
  }
}