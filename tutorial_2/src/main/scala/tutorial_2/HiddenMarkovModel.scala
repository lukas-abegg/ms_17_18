package tutorial_2

import tutorial_2.Helper._

import scala.util.matching.Regex


case class Unigram(key: String, last: String) {
  override def toString: String = key
}

object Unigram {
  def apply(triple: List[String]): Unigram =
    Unigram(triple.head, triple.head)
}


class HiddenMarkovModel {

  private def buildEmissionsInSentence(sentence: List[Unigram], posTags: List[Unigram]): NestedMapType = {
    val emissions = Map(sentence.map { x => x -> Map.empty[Unigram, Double] }: _*)

    (sentence zip posTags).foldLeft(emissions) { (m, x) =>
      val emission = x._1
      val posTag = x._2
      val m2 = m.getOrElse(emission, Map.empty[Unigram, Double])
      val mNew = m2 + ((posTag, m2.getOrElse(posTag, 0.0) + 1.0))
      m + ((emission, mNew))
    }
  }

  private def buildEmissions(sentences: List[List[Unigram]], posTags: List[List[Unigram]]): NestedMapType =
    (sentences zip posTags).foldLeft(Map.empty[Unigram, SimpleMapType]) { (m, x) =>
      val emissions = buildEmissionsInSentence(x._1, x._2)
      merge(m, emissions)
    }

  val startTag = Unigram("#$", "#$")

  private def buildTransitionsInSentence(posTags: List[Unigram]): NestedMapType = {
    val transitions = Map(posTags.map { x => x -> Map.empty[Unigram, Double] }: _*)

    posTags.zipWithIndex.foldLeft(transitions) { (m, x) =>
      x._2 match {
        case i if i > 0 =>
          val tag = x._1
          val prev = posTags(i - 1)
          val m2 = m.getOrElse(tag, Map.empty[Unigram, Double])
          val mNew = m2 + ((prev, m2.getOrElse(prev, 0.0) + 1.0))
          m + ((tag, mNew))
        case _ =>
          val tag = x._1
          val prev = startTag
          val m2 = m.getOrElse(tag, Map.empty[Unigram, Double])
          val mNew = m2 + ((prev, m2.getOrElse(prev, 0.0) + 1.0))
          m + ((tag, mNew))
      }
    }
  }

  private def buildTransitions(posTags: List[List[Unigram]]): NestedMapType =
    posTags.foldLeft(Map.empty[Unigram, SimpleMapType]) { (m, x) =>
      val transitions = buildTransitionsInSentence(x)
      merge(m, transitions)
  }

  private def getTrigrams(r: Regex, sentence: String): List[Unigram] =
    tokenizeLine(r, sentence).sliding(1).map(Unigram(_)).toList

  private lazy val getGrams: Regex = "\\S+(?=\\/\\S+)".r
  private lazy val getPOSTags: Regex = "(?<=\\S\\/)\\S+".r

  private def tokenizeLine(r: Regex, line: String): List[String] =
    r.findAllIn(line).toList

  private var transitions: NestedMapType = _
  private var emissions: NestedMapType = _

  def fit(sentences: List[String]): Unit = {
    val words: List[List[Unigram]] = sentences.map(getTrigrams(getGrams, _))
    val posTags: List[List[Unigram]] = sentences.map(getTrigrams(getPOSTags, _))

    this.transitions = averagedOverAll(buildTransitions(posTags))
    this.emissions = averagedOverAll(buildEmissions(words, posTags))
  }

  def pred(sentence: String): List[Unigram] =
    getTrigrams(getGrams, sentence)

  def validations(sentence: String): List[Annotation] = {
    val words = getTrigrams(getGrams, sentence).reverse
    val posTags = getTrigrams(getPOSTags, sentence).reverse

    words.zipWithIndex.map { word =>
      Annotation(word._1.last, posTags(word._2).last)
    }
  }

  def getEmission(e: Unigram, t: Unigram): Double =
    emissions.getOrElse(e, Map.empty[Unigram, Double]).getOrElse(t, 0.0)

  def getTransition(t: Unigram, prev: Unigram): Double =
    transitions.getOrElse(t, Map.empty[Unigram, Double]).getOrElse(prev, 0.0)

  def getTags: List[Unigram] =
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
