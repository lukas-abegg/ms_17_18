package tutorial_2

import scala.util.matching.Regex

import scalaz.Scalaz._


case class Triple(key: String) {
  override def toString: String = key
}

object Triple {
  def apply(triple: List[String]): Triple =
    Triple(s"${triple.head}_${triple(1)}_${triple.last}")
}


class HiddenMarkovModel {

  private def logOf(emissions: Map[Triple, Map[Triple, Double]]): Map[Triple, Map[Triple, Double]] =
    emissions.map { e =>
      e._2 match {
        case ts: Map[Triple, Double] =>
          (e._1, ts.map { t => (t._1, Math.log(t._2 / e._2.values.sum)) })
      }
    }

  private def buildEmissionsInSentence(sentence: List[Triple], posTags: List[Triple]): Map[Triple, Map[Triple, Double]] = {
    val emissions = Map(sentence.map { x => x -> Map.empty[Triple, Double] }: _*)

    (sentence zip posTags).foldLeft(emissions) { (m, x) =>
      val emission = x._1
      val transition = x._2
      val m2 = m.getOrElse(emission, Map.empty[Triple, Double])
      m + (emission -> (m2 + ((transition, m2.getOrElse(transition, 0.0) + 1))))
    }
  }

  private def buildEmissions(sentences: List[List[Triple]], posTags: List[List[Triple]]): Map[Triple, Map[Triple, Double]] =
    (sentences zip posTags).foldLeft(Map.empty[Triple, Map[Triple, Double]]) { (m, x) =>
      m |+| buildEmissionsInSentence(x._1, x._2)
    }

  private lazy val startTag = Triple("#$")

  private def buildTransitionsInSentence(posTags: List[Triple]): Map[Triple, Map[Triple, Double]] = {
    val transitions = Map(posTags.map { x => x -> Map.empty[Triple, Double] }: _*)

    posTags.zipWithIndex.foldLeft(transitions) { (m, x) =>
      x._2 match {
        case i if i > 0 =>
          val tag = x._1
          val prev = posTags(i - 1)
          val m2 = m.getOrElse(tag, Map.empty[Triple, Double])
          m + ((tag, m2 + ((prev, m2.getOrElse(prev, 0) + 1))))
        case _ =>
          val tag = x._1
          val prev = startTag
          val m2 = m.getOrElse(tag, Map.empty[Triple, Double])
          m + ((tag, m2 + ((prev, m2.getOrElse(prev, 0) + 1))))
      }
    }
  }

  private def buildTransitions(posTags: List[List[Triple]]): Map[Triple, Map[Triple, Double]] =
    posTags.foldLeft(Map.empty[Triple, Map[Triple, Double]]) { (m, x) =>
    m |+| buildTransitionsInSentence(x)
  }

  private def getTrigrams(r: Regex, sentence: String): List[Triple] =
    tokenizeLine(r, sentence).sliding(3).map(Triple(_)).toList

  private lazy val getGrams: Regex = "\\S+(?=\\/\\S+)".r
  private lazy val getPOSTags: Regex = "(?<=\\S\\/)\\S+".r

  private lazy val prefSuffix = List("#$", "#$")

  private def tokenizeLine(r: Regex, line: String): List[String] =
    prefSuffix ++ r.findAllIn(line).toList ++ prefSuffix

  private var transitions: Map[Triple, Map[Triple, Double]] = _
  private var emissions: Map[Triple, Map[Triple, Double]] = _

  def fit(sentences: List[String]): Unit = {
    val words: List[List[Triple]] = sentences.map(getTrigrams(getGrams, _))
    val posTags: List[List[Triple]] = sentences.map(getTrigrams(getPOSTags, _))

    this.transitions = logOf(buildTransitions(posTags))
    this.emissions = logOf(buildEmissions(words, posTags))
  }

  def pred(sentence: String): List[Triple] =
    getTrigrams(getGrams, sentence)

  def getEmission(e: Triple, t: Triple): Double =
    emissions.getOrElse(e, Map.empty[Triple, Double]).getOrElse(t, 0.0)

  def getTransition(t: Triple, prev: Triple): Double =
    transitions.getOrElse(t, Map.empty[Triple, Double]).getOrElse(prev, 0.0)

  def getPOSTags(): List[Triple] =
    transitions.keys.toList
}
