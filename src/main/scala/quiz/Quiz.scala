package quiz

import quiz.Quiz.*
import quiz.data.KanjiData
import quiz.kanji.{Grade, Kanji, Kyu, Level}
import quiz.utils.Choice
import quiz.utils.Choice.Range

import scala.language.implicitConversions
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

  private def frequency(): Boolean = {
    Range('1', '9').get(choice, Map('0' -> "most frequent"), "Frequency buckets of 250") match {
      case x if choice.isQuit(x) => false
      case x =>
        val pos = (x - '0') * FrequencyBlock
        val end = (if (x == '9') data.frequencyList.size else pos + FrequencyBlock) + 1
        makeList(data.frequencyList.slice(pos, end))
    }
  }

  private def grade(): Boolean = {
    Range('1', '6').get(choice, Map('s' -> "Secondary School"), "Grade", 's') match {
      case x if choice.isQuit(x) => false
      case x => makeList(data.gradeMap(if (x == 's') Grade.S else Grade.fromOrdinal(x - '1')))
    }
  }

  private def level(): Boolean = {
    choice.get(Range('1', '5'), "JLPT Level", '1') match {
      case x if choice.isQuit(x) => false
      case x => makeList(data.levelMap(Level.valueOf("N" + x)))
    }
  }

  private def kyu(): Boolean = {
    Range('1', '9')
      .get(choice, Map('0' -> "k10", 'j' -> "Jun-K2", 'k' -> "Jun-K1"), "Kentei Kyu") match {
      case x if choice.isQuit(x) => false
      case x => makeList(data.kyuMap(x match {
          case '0' => Kyu.K10
          case 'j' => Kyu.KJ2
          case 'k' => Kyu.KJ1
          case _ => Kyu.valueOf("K" + x)
        }))
    }
  }

  private def makeList(entries: Vector[Kanji]) = {
    choice.get(ListOrder, "List order", 'r') match {
      case FromBeginning => run(entries)
      case FromEnd => run(entries.reverse)
      case RandomOrder => run(random.shuffle(entries))
      case _ => false
    }
  }

  private def run(entries: Vector[Kanji]) = {
    choice.println(s">>> Starting quiz for ${entries.size} Kanji\n")
    val result = entries.foldLeft(Result()) {
      case (r, _) if r.isQuit => r // once quit has been set then don't modify result anymore
      case (r, _) => r.next(true)
    }
    choice.println(s"\n>>> Final score: $result\n")
    true
  }
}

object Quiz {
  def apply(): Quiz = new Quiz(KanjiData(KanjiData.dataDir()), Choice(), true)

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

  private enum State {
    case MeaningOff, MeaningOn, Quit
  }
  import State.*
  private case class Result(question: Int = 0, score: Int = 0, state: State = MeaningOff) {
    override def toString: String = s"$score/$question"

    def next(correct: Boolean): Result =
      Result(question + 1, if (correct) score + 1 else score, state)

    def flipMeaning: Result =
      Result(question, score, if (state == MeaningOn) MeaningOff else MeaningOn)

    def quit: Result = Result(question, score, Quit)

    def isQuit: Boolean = state == Quit
    def showMeaning: Boolean = state == MeaningOn
  }
}
