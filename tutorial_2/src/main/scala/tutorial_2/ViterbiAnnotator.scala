package tutorial_2


case class Annotation(word: String, posTag: String)


class ViterbiAnnotator(hmm: HiddenMarkovModel) {

  lazy val posTagsDict = List(".", "(", ")", "*", "--", ",", ":", "ABL", "ABN", "ABX", "AP", "AT", "BE", "BED", "BEDZ", "BEG", "BEM", "BEN", "BER",
    "BEZ", "CC", "CD", "CS", "DO", "DOD", "DOZ", "DT", "DTI", "DTS", "DTX", "EX", "FW", "HV", "HVD", "HVG", "HVN", "IN", "JJ", "JJR", "JJS",
    "JJT", "MD", "NC", "NN", "NN$", "NNS", "NNS$", "NP", "NP$", "NPS", "NPS$", "NR", "OD", "PN", "PN$", "PP$", "PP$$", "PPL", "PPLS", "PPO",
    "PPS", "PPSS", "PRP", "PRP$", "QL", "QLP", "RB", "RBR", "RBT", "RN", "RP", "TO", "UH", "VB", "VBD", "VBG", "VBN", "VBP", "VBZ", "WDT",
    "WP$", "WPO", "WPS", "WQL", "WRB").map(_.toLowerCase)

  private def getTagProbs(w: Triple, p: List[String]): Map[Triple, Long] =
    posTagsDict.foldLeft(Map.empty[Triple, Long]) { (m, x) =>
      val t = Triple(p :+ x)
      m + ((t, hmm.getTransition(w, t).toLong))
    }

  private def calcProbs(w: Triple, prevs: Option[Map[Triple, Long]]) = {
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
      var sentenceMatrix = Map[String, Map[Triple, Long]] // Tabelle pro Satz

      triples.map { triple =>
        //var resultColumn = getTagProbs(triple)
      }
    }
  }
}