package quiz

import quiz.Quiz.*
import quiz.data.KanjiData
import quiz.kanji.{Grade, Kanji, Kyu, Level}
import quiz.utils.Choice
import quiz.utils.Choice.Range

import scala.annotation.tailrec
import scala.collection.mutable
import scala.language.{dynamics, implicitConversions, reflectiveCalls}
import scala.util.Random

class Quiz(data: KanjiData, choice: Choice, randomize: Boolean) {
  private val random = Random(if (randomize) System.currentTimeMillis else 0)
  choice.setQuit('q')

  def start(): Unit = while (choice.get(QuizTypes, "Quiz Type", LevelQuiz) match {
      case FrequencyQuiz => frequency()
      case GradeQuiz => grade()
      case LevelQuiz => level()
      case KyuQuiz => frequency()
      case x => !choice.isQuit(x)
    }) {}

  private def frequency(): Boolean = Range('1', '9').get(choice, FrequencyMap, FrequencyMsg) match {
    case x if choice.isQuit(x) => false
    case x =>
      val pos = (x - '0') * FrequencyBlock
      val end = (if (x == '9') data.frequencyList.size else pos + FrequencyBlock) + 1
      makeList(data.frequencyList.slice(pos, end))
  }

  private def grade(): Boolean = Range('1', '6').get(choice, GradeMap, "Grade", 's') match {
    case x if choice.isQuit(x) => false
    case x => makeList(data.gradeMap(if (x == 's') Grade.S else Grade.fromOrdinal(x - '1')))
  }

  private def level(): Boolean = choice.get(Range('1', '5'), "JLPT Level", '1') match {
    case x if choice.isQuit(x) => false
    case x => makeList(data.levelMap(Level.valueOf("N" + x)))
  }

  private def kyu(): Boolean = Range('1', '9').get(choice, KyuMap, "Kentei Kyu") match {
    case x if choice.isQuit(x) => false
    case x => makeList(data.kyuMap(x match {
        case '0' => Kyu.K10
        case 'j' => Kyu.KJ2
        case 'k' => Kyu.KJ1
        case _ => Kyu.valueOf("K" + x)
      }))
  }

  private def makeList(entries: Vector[Kanji]) = choice.get(ListOrder, "List order", 'r') match {
    case FromBeginning => run(entries)
    case FromEnd => run(entries.reverse)
    case RandomOrder => run(random.shuffle(entries))
    case _ => false
  }

  private def run(entries: Vector[Kanji]) = {
    val result = entries.foldLeft(Result()) {
      case (r, _) if r.isQuit => r // once quit has been set don't modify result anymore
      case (r, k) => question(entries, k, r)
    }
    choice.println(s"\n>>> Final score: $result\n")
    !result.isQuit
  }

  private def question(entries: Vector[Kanji], k: Kanji, result: Result) = {
    val choices = mutable.Set(result.question)
    while (choices.size < 4) choices.add(Random.nextInt(entries.size))
    val answers = choices.toArray

    @tailrec
    def f(r: Result): Result = {
      choice.println(r.questionMsg(entries.size, k))
      answers.zipWithIndex.foreach { case (answer, index) =>
        choice.println(s"${index + 1}: " + entries(answer).reading)
      }
      Range('1', '4').get(choice, Map(FlipMeaning -> r.meaningMsg), "Choose reading") match {
        case FlipMeaning => f(r.flipMeaning)
        case x if choice.isQuit(x) => r.quit
        case x => r.next(answers(x - '1') == r.question)
      }
    }
    f(result)
  }
}

object Quiz {
  def apply(): Quiz = new Quiz(KanjiData(KanjiData.dataDir()), Choice(), true)

  private val FlipMeaning = '-'

  // Quiz Types
  private val FrequencyQuiz = 'f'
  private val GradeQuiz = 'g'
  private val LevelQuiz = 'l'
  private val KyuQuiz = 'k'
  private val QuizTypes =
    Map(FrequencyQuiz -> "Frequency", GradeQuiz -> "Grade", LevelQuiz -> "Level", KyuQuiz -> "Kyu")
  // List Order
  private val FromBeginning = 'b'
  private val FromEnd = 'e'
  private val RandomOrder = 'r'
  private val ListOrder =
    Map(FromBeginning -> "from beginning", FromEnd -> "from end", RandomOrder -> "random")
  // Frequency Quiz
  private val FrequencyBlock = 250
  private val FrequencyMap = Map('0' -> "most frequent")
  private val FrequencyMsg = s"Frequency buckets of $FrequencyBlock"

  private val GradeMap = Map('s' -> "Secondary School")
  private val KyuMap = Map('0' -> "k10", 'j' -> "Jun-K2", 'k' -> "Jun-K1")

  private enum State {
    case MeaningOff, MeaningOn, Quit
  }
  import State.*
  private case class Result(question: Int = 0, score: Int = 0, state: State = MeaningOff) {
    /** return string representation of this Result that includes the current score and question */
    override def toString: String = s"$score/$question"

    /** return a message for a quiz question that includes the question number, current score and
     *  Kanji being tested (optionally followed by the Kanji meaning depending on `state`)
     */
    def questionMsg(size: Int, k: Kanji): String =
      s"\nQuestion ${question + 1} of $size (score $score/${question - score}): Kanji ${k.name}" +
        (if (state == MeaningOn) s" - ${k.meaning}" else "")

    /** return a message for flipping meaning based on the current value of `state` */
    def meaningMsg: String = (if (state == MeaningOn) "hide" else "show") + " meanings"

    /** return a Result instance for the next question
     *  @param correct if true then increment `score`
     */
    def next(correct: Boolean): Result =
      Result(question + 1, if (correct) score + 1 else score, state)

    /** return a Result instance with 'meaning' state flipped, i.e., if state is `MeaningOn`
     *  the set it to `MeaningOff` and vice versa
     */
    def flipMeaning: Result =
      Result(question, score, if (state == MeaningOn) MeaningOff else MeaningOn)

    /** return a Result instance with state set to `Quit` */
    def quit: Result = Result(question, score, Quit)

    /** return true if state is `Quit` */
    def isQuit: Boolean = state == Quit
  }
}
