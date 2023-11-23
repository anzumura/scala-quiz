package quiz

import quiz.Choice._

import java.io.{BufferedReader, InputStream, InputStreamReader, PrintStream}
import scala.annotation.tailrec
import scala.collection.immutable.TreeMap
import scala.language.implicitConversions

class Choice private (private var _quit: Option[Char] = None,
    var quitDescription: String = DefaultQuitDescription,
    val is: InputStream = System.in, private val os: PrintStream = System.out) {
  // call setQuit to force validation upon construction
  _quit.foreach(setQuit)
  private val reader = new BufferedReader(new InputStreamReader(is))

  def quit: Option[Char] = _quit
  def quit_=(q: Option[Char]): Unit = {
    q.foreach(x => checkChoice(x, "quit option"))
    _quit = q
  }

  // helper methods to avoid using Option
  def isQuit(q: Char): Boolean = quit.contains(q)
  def hasQuit: Boolean = quit.nonEmpty
  def clearQuit(): Unit = quit = None
  def setQuit(q: Char): Unit = quit = Option(q)
  def setQuit(q: Char, d: String): Unit = {
    quit = Option(q)
    quitDescription = d
  }

  /**
   * read in a choice from `is` provided in the ctor
   * @param msg beginning part of prompt message written to `os` specified in
   *            the ctor, the rest of the prompt message shows the choices
   * @param useQuit if 'QuitOff' then 'quit option' is not included in choices
   * @param choices set of choices the user must choose from
   * @param defaultChoice optional default choice (for just pressing return)
   * @return one of the choices from `choices` or possibly the 'quit option'
   * @throws DomainException if `def` is provided, but it's not in `choicesIn`
   * @throws DomainException if 'quit option' is set and is also in `choices`
   * @throws DomainException if any choice in `choices` is not printable Ascii
   */
  def get(msg: String, choices: Choices, defaultChoice: Option[Char] = None,
      useQuit: UseQuit = QuitOn): Char = {
    val c = quit.map(q =>
      if (useQuit) {
        if (choices.contains(q)) error(s"quit option '$q' already in choices")
        choices.updated(q, quitDescription)
      } else choices
    ).getOrElse(choices)
    if (c.isEmpty) error("must specify at least one choice")
    getChoice(getPrompt(msg, c, defaultChoice), c, defaultChoice)
  }

  // 'get' overloads to avoid forcing the caller to use Option[defaultChoice)
  def get(msg: String, choices: Choices, defaultChoice: Char): Char =
    get(msg, choices, Option(defaultChoice))
  def get(msg: String, choices: Choices, useQuit: UseQuit): Char =
    get(msg, choices, None, useQuit)
  def get(msg: String, choices: Choices, defaultChoice: Char, useQuit: UseQuit)
      : Char = get(msg, choices, Option(defaultChoice), useQuit)
  // 'get' overloads that don't require 'msg'
  def get(choices: Choices): Char = get("", choices, None)
  def get(choices: Choices, defaultChoice: Char): Char =
    get("", choices, Option(defaultChoice))
  def get(choices: Choices, useQuit: UseQuit): Char =
    get("", choices, None, useQuit)
  def get(choices: Choices, defaultChoice: Char, useQuit: UseQuit): Char =
    get("", choices, Option(defaultChoice), useQuit)

  @tailrec
  private def getChoice(prompt: String, choices: Choices,
      defaultChoice: Option[Char] = None): Char = {
    (read(prompt), defaultChoice) match {
      case (Some(choice), _) if choices.contains(choice) => choice
      case (None, Some(choice)) => choice
      case _ => getChoice(prompt, choices, defaultChoice)
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
    val completeRange = () =>
      rangeStart.foreach(c => if (c != prevChoice) prompt ++= s"-$prevChoice")
    // use TreeMap to traverse entries in alphabetical order
    (choices to TreeMap).foreach { case (choice, description) =>
      checkChoice(choice, "option")
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
        if (!choices.contains(s)) error(s"default option '$s' not in choices")
        prompt ++= s") def '$s': "
      case _ => prompt ++= "): "
    }
    prompt.result()
  }
}

object Choice {
  type Choices = Map[Char, String]
  def Choices(xs: (Char, String)*): Choices = Map(xs: _*)

  val DefaultQuitDescription = "quit"

  sealed trait UseQuit
  case object QuitOn extends UseQuit
  case object QuitOff extends UseQuit
  implicit def toBoolean(x: UseQuit): Boolean = x eq QuitOn

  def apply(): Choice = new Choice
  def apply(q: Char, d: String = DefaultQuitDescription): Choice =
    new Choice(Option(q), d)
  // used by test code
  def apply(is: InputStream, os: PrintStream): Choice =
    new Choice(is = is, os = os)

  private def checkChoice(c: Char, msg: String): Unit = {
    if (c < ' ' || c > '~') error(s"invalid $msg: '0x${c.toInt.toHexString}'")
  }

  private def error(msg: String) = throw DomainException(msg)
}
