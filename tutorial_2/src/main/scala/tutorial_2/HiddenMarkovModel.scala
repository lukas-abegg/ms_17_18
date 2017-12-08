package tutorial_2

import tutorial_2.Helper._

import scala.util.matching.Regex


case class Bigram(key: String, last: String) {
  override def toString: String = key
}

object Bigram {
  def apply(bigram: List[String]): Bigram =
    Bigram(s"${bigram.head}_${bigram.last}", bigram.last)
}


class HiddenMarkovModel {

  private def buildEmissionsInSentence(sentence: List[Bigram], posTags: List[Bigram]): NestedMapType = {
    val emissions = Map(sentence.map { x => x -> Map.empty[Bigram, Double] }: _*)

    (sentence zip posTags).foldLeft(emissions) { (m, x) =>
      val emission = x._1
      val transition = x._2
      val m2 = m.getOrElse(emission, Map.empty[Bigram, Double])
      m + (emission -> (m2 + ((transition, m2.getOrElse(transition, 0.0) + 1))))
    }
  }

  private def buildEmissions(sentences: List[List[Bigram]], posTags: List[List[Bigram]]): NestedMapType =
    (sentences zip posTags).foldLeft(Map.empty[Bigram, SimpleMapType]) { (m, x) =>
      merge(m, buildEmissionsInSentence(x._1, x._2))
    }

  val startTag = Bigram("#$", "#$")

  private def buildTransitionsInSentence(posTags: List[Bigram]): NestedMapType = {
    val transitions = Map(posTags.map { x => x -> Map.empty[Bigram, Double] }: _*)

    posTags.zipWithIndex.foldLeft(transitions) { (m, x) =>
      x._2 match {
        case i if i > 0 =>
          val tag = x._1
          val prev = posTags(i - 1)
          val m2 = m.getOrElse(tag, Map.empty[Bigram, Double])
          m + ((tag, m2 + ((prev, m2.getOrElse(prev, 0.0) + 1.0))))
        case _ =>
          val tag = x._1
          val prev = startTag
          val m2 = m.getOrElse(tag, Map.empty[Bigram, Double])
          m + ((tag, m2 + ((prev, m2.getOrElse(prev, 0.0) + 1.0))))
      }
    }
  }

  private def buildTransitions(posTags: List[List[Bigram]]): NestedMapType =
    posTags.foldLeft(Map.empty[Bigram, SimpleMapType]) { (m, x) =>
      merge(m, buildTransitionsInSentence(x))
  }

  private def getUnigrams(r: Regex, sentence: String): List[Bigram] =
    tokenizeLine(r, sentence).sliding(2).map(Bigram(_)).toList

  private lazy val getGrams: Regex = "\\S+(?=\\/\\S+)".r
  private lazy val getPOSTags: Regex = "(?<=\\S\\/)\\S+".r

  private lazy val prefSuffix = List("#$")

  private def tokenizeLine(r: Regex, line: String): List[String] =
    prefSuffix ++ r.findAllIn(line).toList ++ prefSuffix

  private var transitions: NestedMapType = _
  private var emissions: NestedMapType = _

  def fit(sentences: List[String]): Unit = {
    val words: List[List[Bigram]] = sentences.map(getUnigrams(getGrams, _))
    val posTags: List[List[Bigram]] = sentences.map(getUnigrams(getPOSTags, _))

    this.transitions = averagedOverAll(buildTransitions(posTags))
    this.emissions = averagedOverAll(buildEmissions(words, posTags))
  }

  def pred(sentence: String): List[Bigram] =
    getUnigrams(getGrams, sentence)

  def validations(sentence: String): List[Annotation] = {
    val words = getUnigrams(getGrams, sentence).reverse
    val posTags = getUnigrams(getPOSTags, sentence).reverse

    words.zipWithIndex.map { word =>
      Annotation(word._1.last, posTags(word._2).last)
    }
  }

  def getEmission(e: Bigram, t: Bigram): Double =
    emissions.getOrElse(e, Map.empty[Bigram, Double]).getOrElse(t, 0.0)

  def getTransition(t: Bigram, prev: Bigram): Double =
    transitions.getOrElse(t, Map.empty[Bigram, Double]).getOrElse(prev, 0.0)

  def getTags: List[Bigram] =
    transitions.filterKeys(_ != startTag).keys.toList

  def loadModel(parser: Parser): Unit = {
    val model: Model = parser.loadModelFromFile()
    this.transitions = model.transitions
    this.emissions = model.emissions
  }

  def saveModel(parser: Parser): Unit = {
    val model = Model(emissions, transitions)
    parser.writeModelToFile(model)
  }
}
