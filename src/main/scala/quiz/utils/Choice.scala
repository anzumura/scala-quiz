package quiz.utils

import quiz.utils.Choice.*

import java.io.{BufferedReader, InputStream, InputStreamReader, PrintStream}
import scala.annotation.tailrec
import scala.collection.immutable.TreeMap
import scala.language.implicitConversions

class Choice private (
    private var _quit: Option[Char] = None, var quitDescription: String = DefaultQuitDescription,
    val is: InputStream = System.in, private val os: PrintStream = System.out
) extends ThrowsDomainException {
  /** returns the current `quit` option or None is no option has been set */
  def quit: Option[Char] = _quit

  /** sets `quit` option */
  def quit_=(q: Option[Char]): Unit = {
    q.foreach(x => checkChoice(x, "[Choice] quit"))
    _quit = q
  }

  // helper methods to avoid using an Option[Char] in above quit methods
  def isQuit(q: Char): Boolean = quit.contains(q)
  def hasQuit: Boolean = quit.nonEmpty
  def clearQuit(): Unit = quit = None
  def setQuit(q: Char): Unit = quit = Option(q)
  def setQuit(q: Char, d: String): Unit = {
    quit = Option(q)
    quitDescription = d
  }

  /** reads in a choice from `is` provided in the ctor
   *  @param choices set of choices the user must choose from
   *  @param msg beginning part of prompt message written to `os` specified in the ctor, the rest
   *             of the prompt message shows the choices
   *  @param useQuit if 'QuitOff' then 'quit option' is not included in choices
   *  @param defaultChoice optional default choice (for just pressing return)
   *  @return one of the choices from `choices` or possibly the 'quit option'
   *  @throws DomainException if `def` is provided, but it's not in `choicesIn`
   *  @throws DomainException if 'quit option' is set and is also in `choices`
   *  @throws DomainException if any choice in `choices` is not printable Ascii
   */
  def get(
      choices: Choices, msg: String = "", useQuit: UseQuit = UseQuit.Yes,
      defaultChoice: Option[Char] = None
  ): Char = {
    val c = quit.map(q =>
      if (useQuit) {
        if (choices.contains(q)) domainError(s"quit option '$q' already in choices")
        choices.updated(q, quitDescription)
      } else choices
    ).getOrElse(choices)
    if (c.isEmpty) domainError("must specify at least one choice")
    getChoice(getPrompt(msg, c, defaultChoice), c, defaultChoice)
  }

  // 'get' overloads using 'd' Char for default choice instead of Option[Char]
  def get(c: Choices, m: String, d: Char): Char = get(c, m, UseQuit.Yes, Option(d))
  def get(c: Choices, m: String, d: Char, u: UseQuit): Char = get(c, m, u, Option(d))
  // overloads without 'm' message parameter (defaults to empty string)
  def get(c: Choices, u: UseQuit): Char = get(c, "", u)
  def get(c: Choices, d: Char): Char = get(c, "", d)
  def get(c: Choices, d: Char, u: UseQuit): Char = get(c, "", d, u)

  @tailrec
  private def getChoice(prompt: String, c: Choices, d: Option[Char]): Char = {
    (read(prompt), d) match {
      case (Some(choice), _) if c.contains(choice) => choice
      case (None, Some(choice)) => choice
      case _ => getChoice(prompt, c, d)
    }
  }

  @tailrec
  private def read(prompt: String): Option[Char] = {
    os.print(prompt)
    os.flush()
    reader.readLine() match {
      case line if line.isEmpty => None
      case line if line.length == 1 => Option(line(0))
      case _ => read(prompt)
    }
  }

  private def getPrompt(msg: String, choices: Choices, d: Option[Char]) = {
    val prompt = new StringBuilder(if (msg.isEmpty) "(" else s"$msg (")
    var rangeStart = Option.empty[Char]
    var prevChoice: Char = 0
    val completeRange =
      () => rangeStart.foreach(c => if (c != prevChoice) prompt ++= s"-$prevChoice")
    // use TreeMap to traverse entries in alphabetical order
    (choices to TreeMap).foreach { case (choice, description) =>
      checkChoice(choice, "[Choice] option")
      if (description.isEmpty) {
        if (rangeStart.isEmpty) {
          if (prevChoice > 0) prompt ++= ", "
          prompt += choice
          rangeStart = Option(choice) // start new range
        } else if (choice - prevChoice > 1) {
          completeRange() // complete range if jump in option values is > 1
          prompt ++= s", $choice"
          rangeStart = Option(choice)
        } // continue processing consecutive values (so don't update rangeStart)
      } else {
        if (rangeStart.nonEmpty) {
          completeRange()
          rangeStart = None
        }
        if (prevChoice > 0) prompt ++= ", "
        prompt ++= s"$choice=$description"
      }
      prevChoice = choice
    }
    completeRange()
    d match {
      case Some(s) =>
        if (!choices.contains(s)) domainError(s"default option '$s' not in choices")
        prompt ++= s") def '$s': "
      case _ => prompt ++= "): "
    }
    prompt.result()
  }

  // call setQuit to force validation upon construction
  _quit.foreach(setQuit)
  private val reader = new BufferedReader(new InputStreamReader(is))
}

object Choice {
  type Choices = Map[Char, String]
  def Choices(xs: (Char, String)*): Choices = Map(xs: _*)

  /** inclusive range of choices with empty descriptions that can be used in `Choice.get` methods
   *  . For example:
   *  <pre>
   *     val c = Choice()
   *     Range('1', '6').get(c, "grade", Map('h' -> "high"))
   *  </pre>
   *  results in a prompt of "grade (1-6, h=high): "
   */
  case class Range(start: Char, end: Char) extends ThrowsDomainException {
    checkChoice(start, "[Range] start")
    checkChoice(end, "[Range] end")
    if (start > end) domainError(s"start '$start' greater than end '$end'")

    // call 'Choice.get' methods with this range merged into 'Choices'
    def get(o: Choice, c: Choices, m: String = "", u: UseQuit = UseQuit.Yes): Char = o
      .get(merge(c), m, u)
    def get(o: Choice, c: Choices, m: String, d: Char): Char = o.get(merge(c), m, d)
    def get(o: Choice, c: Choices, m: String, d: Char, u: UseQuit): Char = o.get(merge(c), m, d, u)
    def get(o: Choice, c: Choices, u: UseQuit): Char = o.get(merge(c), u)
    def get(o: Choice, c: Choices, d: Char): Char = o.get(merge(c), d)
    def get(o: Choice, c: Choices, d: Char, u: UseQuit): Char = o.get(merge(c), d, u)

    private def merge(c: Choices) = {
      var result = c
      (start to end).foreach(c =>
        result = result.updatedWith(c) {
          case None => Option("")
          case _ => domainError(s"option '$c' already in choices")
        }
      )
      result
    }
  }

  // implicit conversion to allow calling 'Choice.get' methods  with 'Range' instead of 'Choices'
  implicit def toChoices(r: Range): Choices = Choices((r.start to r.end).map(c => (c, "")): _*)

  val DefaultQuitDescription = "quit"

  enum UseQuit {
    case Yes, No
  }
  given Conversion[UseQuit, Boolean] = (x: UseQuit) => x eq UseQuit.Yes

  def apply(): Choice = new Choice
  def apply(q: Char, d: String = DefaultQuitDescription): Choice = new Choice(Option(q), d)
  // used by test code
  def apply(is: InputStream, os: PrintStream): Choice = new Choice(is = is, os = os)

  private def checkChoice(c: Char, msg: String): Unit = {
    if (c < ' ' || c > '~') throw DomainException(s"$msg is invalid: '0x${c.toInt.toHexString}'")
  }
}
