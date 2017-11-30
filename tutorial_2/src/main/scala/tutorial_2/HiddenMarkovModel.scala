package tutorial_2

import scala.util.matching.Regex


case class Triple(key: String, prev: List[String]) {
  override def toString: String = key
}

object Triple {
  def apply(triple: List[String]): Triple =
    Triple(s"${triple.head}_${triple(1)}_${triple.last}", triple.take(2))
}

case class Transition(key: String) {
  override def toString: String = key
}

object Transition {
  def apply(wordTriple: Triple, posTriple: Triple): Transition =
    Transition(s"${wordTriple}_${posTriple}")
}


class HiddenMarkovModel {

  private def buildPredictables(words: List[List[String]]): List[Triple] = words.map(Triple(_))

  private def buildTransitions(words: List[List[String]], posTags: List[List[String]]): Map[Transition, Int] =
    (words zip posTags).foldLeft(Map.empty[Transition, Int]) { (m, x) =>
      val transition = Transition(Triple(x._1), Triple(x._2))
      m + ((transition, m.getOrElse(transition, 0) + 1))
    }

  private def buildTriples(elements: List[List[String]]): Map[Triple, Int] =
    elements.foldLeft(Map.empty[Triple, Int]) { (m, x) =>
      val emission = Triple(x)
      m + ((emission, m.getOrElse(emission, 0) + 1))
    }

  private def getTrigrams(r: Regex, lines: List[String]): List[List[String]] =
    lines.flatMap(line => tokenizeLine(r, line).sliding(3).toList)

  private lazy val getGrams: Regex = "\\S+(?=\\/\\S+)".r
  private lazy val getPOSTags: Regex = "(?<=\\S\\/)\\S+".r

  private lazy val prefSuffix = List("#$", "#$")

  private def tokenizeLine(r: Regex, line: String): List[String] =
    prefSuffix ++ r.findAllIn(line).toList ++ prefSuffix

  private var trainWords: Map[Triple, Int] = _
  private var emissions: Map[Triple, Int] = _
  private var transitions: Map[Transition, Int] = _

  def fit(sentences: List[String]) = {
    val words: List[List[String]] = getTrigrams(getGrams, sentences)
    val posTags: List[List[String]] = getTrigrams(getPOSTags, sentences)

    this.trainWords = buildTriples(words)
    this.emissions = buildTriples(posTags)
    this.transitions = buildTransitions(words, posTags)
  }

  def pred(sentence: String) =
    buildPredictables(getTrigrams(getGrams, List(sentence)))

  def getTransition(x: Triple, e: Triple) =
    transitions.getOrElse(Transition(x, e), 0)

  def getEmission(e: Triple) =
    emissions.getOrElse(e, 0)
}
