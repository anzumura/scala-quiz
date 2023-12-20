package quiz

import quiz.Quiz.*
import quiz.data.KanjiData
import quiz.kanji.Kanji.Info
import quiz.kanji.{Grade, Kanji, Kyu, Level}
import quiz.utils.Choice
import quiz.utils.Choice.Range

import scala.annotation.tailrec
import scala.collection.immutable.TreeMap
import scala.language.implicitConversions
import scala.util.Random

class Quiz(data: KanjiData, choice: Choice, randomize: Boolean) {
  private val random = Random(if (randomize) System.currentTimeMillis else 0)
  choice.setQuit('q')

  def start(): Unit = while (choice.get(QuizTypes, "Quiz Type", FrequencyQuiz) match {
      case FrequencyQuiz => frequency()
      case GradeQuiz => grade()
      case LevelQuiz => level()
      case KyuQuiz => kyu()
      case x => !choice.isQuit(x)
    }) {}

  private def frequency(): Boolean =
    Range('1', '9').get(choice, FrequencyMap, FrequencyMsg, MostFrequent) match {
      case x if choice.isQuit(x) => false
      case x =>
        val pos = (x - MostFrequent) * FrequencyBlock
        val end = if (x == LeastFrequent) data.frequencyList.size else pos + FrequencyBlock
        makeList(data.frequencyList.slice(pos, end), Info.Frequency)
    }

  private def grade(): Boolean = Range('1', '6').get(choice, GradeMap, "Grade", GradeS) match {
    case x if choice.isQuit(x) => false
    case x =>
      makeList(data.gradeMap(if (x == GradeS) Grade.S else Grade.fromOrdinal(x - '1')), Info.Grade)
  }

  private def level(): Boolean = choice.get(Range('1', '5'), "JLPT Level", '1') match {
    case x if choice.isQuit(x) => false
    case x => makeList(data.levelMap(Level.valueOf("N" + x)), Info.Level)
  }

  private def kyu(): Boolean = Range('1', '9').get(choice, KyuMap, "Kentei Kyu", K10) match {
    case x if choice.isQuit(x) => false
    case x => makeList(data.kyuMap(x match {
          case K10 => Kyu.K10
          case KJ2 => Kyu.KJ2
          case KJ1 => Kyu.KJ1
          case _ => Kyu.valueOf("K" + x)
        }), Info.Kyu)
  }

  private def makeList(entries: Vector[Kanji], exclude: Info) =
    choice.get(ListOrderMap, "List order", ListOrder.Beginning.value) match {
      case ListOrder.Beginning.value => run(entries, exclude)
      case ListOrder.End.value => run(entries.reverse, exclude)
      case ListOrder.Random.value => run(random.shuffle(entries), exclude)
      case _ => false
    }

  private def run(entries: Vector[Kanji], exclude: Info) = {
    val state = entries.foldLeft(State(exclude)) {
      case (r, _) if r.isQuit => r // once quit has been set don't modify result anymore
      case (r, k) => question(entries, k, r)
    }
    choice.println(s"\n>>> Final score: $state\n")
    !state.isQuit
  }

  private def question(entries: Vector[Kanji], k: Kanji, state: State) = {
    // create array of answers that contains the correct answer plus 3 other answers with different
    // readings - use TreeMap so that answers are sorted in 'reading' order
    val answers = Iterator.iterate(TreeMap(k.reading -> state.question)) { m =>
      val answer = random.nextInt(entries.size)
      m + (entries(answer).reading -> answer)
    }.dropWhile(_.size < ChoicesPerQuestion).next().values.toArray

    @tailrec
    def f(s: State): State = {
      choice.println(s.questionMsg(entries.size, k))
      answers.zipWithIndex.foreach { case (answer, index) =>
        choice.println(s"  ${index + 1}: " + entries(answer).reading)
      }
      QuestionRange.get(choice, Map(FlipMeaning -> s.meaningMsg), "Choose reading") match {
        case FlipMeaning => f(s.flipMeaning)
        case x if choice.isQuit(x) => s.quit
        case x if answers(x - '1') == s.question => s.correct
        case _ =>
          choice.println(s"  Incorrect: the answer is ${answers.indexOf(s.question) + 1}")
          s.incorrect(k.name)
      }
    }
    f(state)
  }
}

object Quiz {
  def apply(): Quiz = new Quiz(KanjiData(KanjiData.dataDir()), Choice(), true)

  private val FlipMeaning = '-'
  private val ChoicesPerQuestion = 4
  private val QuestionRange = Range('1', ('0' + ChoicesPerQuestion).asInstanceOf[Char])

  // Quiz Types
  private val FrequencyQuiz = 'f'
  private val GradeQuiz = 'g'
  private val LevelQuiz = 'l'
  private val KyuQuiz = 'k'
  private val QuizTypes =
    Map(FrequencyQuiz -> "Frequency", GradeQuiz -> "Grade", LevelQuiz -> "Level", KyuQuiz -> "Kyu")
  // List Order
  enum ListOrder(val value: Char) {
    case Beginning extends ListOrder('b')
    case End extends ListOrder('e')
    case Random extends ListOrder('r')
  }
  private val ListOrderMap = Map(ListOrder.Beginning.value -> "from beginning",
    ListOrder.End.value -> "from end", ListOrder.Random.value -> "random")
  // Frequency Quiz
  private val FrequencyBlock = 250
  private val MostFrequent = '0'
  private val LeastFrequent = '9'
  private val FrequencyMap = Map(MostFrequent -> "most frequent")
  private val FrequencyMsg = s"Frequency buckets of $FrequencyBlock"
  // Grade Quiz
  private val GradeS = 's' // Secondary School
  private val GradeMap = Map(GradeS -> "Secondary School")
  // Kyu Quiz
  private val K10 = '0'
  private val KJ2 = 'j'
  private val KJ1 = 'k'
  private val KyuMap = Map(K10 -> "k10", KJ2 -> "Jun-K2", KJ1 -> "Jun-K1")

  private object State {
    def apply(exclude: Info): State = new State(exclude)

    enum States {
      case MeaningOff, MeaningOn, Quit
    }
  }
  import State.States
  import State.States.*
  private final class State private (exclude: Info, val question: Int = 0,
      mistakes: Vector[String] = Vector[String](), state: States = States.MeaningOff) {
    /** return string that includes the current score and list of mistakes */
    override def toString: String = s"$score/${if (state == Quit) question else question + 1}" +
      (if (mistakes.isEmpty) "" else mistakes.mkString(" mistakes: ", ", ", ""))

    /** return a message for a quiz question that includes the question number, current score and
     *  Kanji being tested (optionally followed by the Kanji meaning depending on `state`)
     */
    def questionMsg(size: Int, k: Kanji): String =
      s"\nQuestion ${question + 1} of $size (score $score): " + k.info(exclude) +
        (if (state == MeaningOn) s" - ${k.meaning}" else "")

    /** return a message for flipping meaning based on the current value of `state` */
    def meaningMsg: String = (if (state == MeaningOn) "hide" else "show") + " meanings"

    /** return a State instance for the next question */
    def correct: State = new State(exclude, question + 1, mistakes, state)

    /** return a State instance for the next question
     *  @param mistake the Kanji for the current question that was answered incorrectly
     */
    def incorrect(mistake: String): State =
      new State(exclude, question + 1, mistakes :+ mistake, state)

    /** return a copy of this State instance with 'meaning' flipped, i.e., if state is `MeaningOn`
     *  then set it to `MeaningOff` and vice versa
     */
    def flipMeaning: State =
      new State(exclude, question, mistakes, if (state == MeaningOn) MeaningOff else MeaningOn)

    /** return a copy of this State instance with state set to `Quit` */
    def quit: State = new State(exclude, question, mistakes, Quit)

    /** return true if `state` is `Quit` */
    def isQuit: Boolean = state == Quit

    private def score = question - mistakes.size
  }
}
