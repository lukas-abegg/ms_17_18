package tutorial_2

import tutorial_2.Helper._

import scala.util.matching.Regex



case class Model(emissions: NestedMapType, transitions: NestedMapType)

case class Triple(key: String, last: String) {
  override def toString: String = key
}

object Triple {
  def apply(triple: List[String]): Triple =
    Triple(s"${triple.head}_${triple(1)}_${triple.last}", triple.last)
}


class HiddenMarkovModel {

  private def buildEmissionsInSentence(sentence: List[Triple], posTags: List[Triple]): NestedMapType = {
    val emissions = Map(sentence.map { x => x -> Map.empty[Triple, Double] }: _*)

    (sentence zip posTags).foldLeft(emissions) { (m, x) =>
      val emission = x._1
      val transition = x._2
      val m2 = m.getOrElse(emission, Map.empty[Triple, Double])
      m + (emission -> (m2 + ((transition, m2.getOrElse(transition, 0.0) + 1))))
    }
  }

  private def buildEmissions(sentences: List[List[Triple]], posTags: List[List[Triple]]): NestedMapType =
    (sentences zip posTags).foldLeft(Map.empty[Triple, SimpleMapType]) { (m, x) =>
      merge(m, buildEmissionsInSentence(x._1, x._2))
    }

  val startTag = Triple("#$", "#$")

  private def buildTransitionsInSentence(posTags: List[Triple]): NestedMapType = {
    val transitions = Map(posTags.map { x => x -> Map.empty[Triple, Double] }: _*)

    posTags.zipWithIndex.foldLeft(transitions) { (m, x) =>
      x._2 match {
        case i if i > 0 =>
          val tag = x._1
          val prev = posTags(i - 1)
          val m2 = m.getOrElse(tag, Map.empty[Triple, Double])
          m + ((tag, m2 + ((prev, m2.getOrElse(prev, 0.0) + 1.0))))
        case _ =>
          val tag = x._1
          val prev = startTag
          val m2 = m.getOrElse(tag, Map.empty[Triple, Double])
          m + ((tag, m2 + ((prev, m2.getOrElse(prev, 0.0) + 1.0))))
      }
    }
  }

  private def buildTransitions(posTags: List[List[Triple]]): NestedMapType =
    posTags.foldLeft(Map.empty[Triple, SimpleMapType]) { (m, x) =>
      merge(m, buildTransitionsInSentence(x))
  }

  private def getTrigrams(r: Regex, sentence: String): List[Triple] =
    tokenizeLine(r, sentence).sliding(3).map(Triple(_)).toList

  private lazy val getGrams: Regex = "\\S+(?=\\/\\S+)".r
  private lazy val getPOSTags: Regex = "(?<=\\S\\/)\\S+".r

  private lazy val prefSuffix = List("#$", "#$")

  private def tokenizeLine(r: Regex, line: String): List[String] =
    prefSuffix ++ r.findAllIn(line).toList ++ prefSuffix

  private var transitions: NestedMapType = _
  private var emissions: NestedMapType = _

  def fit(sentences: List[String]): Unit = {
    val words: List[List[Triple]] = sentences.map(getTrigrams(getGrams, _))
    val posTags: List[List[Triple]] = sentences.map(getTrigrams(getPOSTags, _))

    this.transitions = logOf(buildTransitions(posTags))
    this.emissions = logOf(buildEmissions(words, posTags))
  }

  def pred(sentence: String): List[Triple] =
    getTrigrams(getGrams, sentence)

  def validations(sentence: String): List[Annotation] = {
    val words = getTrigrams(getGrams, sentence).reverse
    val posTags = getTrigrams(getPOSTags, sentence).reverse

    words.zipWithIndex.map { word =>
      Annotation(word._1.last, posTags(word._2).last)
    }
  }

  def getEmission(e: Triple, t: Triple): Double =
    emissions.getOrElse(e, Map.empty[Triple, Double]).getOrElse(t, 0.0)

  def getTransition(t: Triple, prev: Triple): Double =
    transitions.getOrElse(t, Map.empty[Triple, Double]).getOrElse(prev, 0.0)

  def getTags(): List[Triple] =
    transitions.filterKeys(_ != startTag).keys.toList

  def getModel(): Model = Model(emissions, transitions)
}
