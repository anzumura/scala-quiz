package quiz

import quiz.Choice._

import java.io.{InputStream, PrintStream}

class Choice private (private val os: PrintStream = System.out,
    private val is: Option[InputStream] = None,
    private var quitIn: Option[Char] = None,
    var quitDescription: String = DefaultQuitDescription) {
  private var _quit: Option[Char] = None

  def quit: Option[Char] = _quit
  def quit_=(q: Option[Char]): Unit = {
    q.foreach(x => checkChoice(x, "quit option"))
    _quit = q
  }
  // use public setter for initializing 'quit' to get consistent validation
  quit = quitIn

  // helper methods to avoid using Option
  def isQuit(q: Char): Boolean = quit.contains(q)
  def hasQuit: Boolean = quit.nonEmpty
  def clearQuit(): Unit = quit = None
  def setQuit(q: Char): Unit = quit = Option(q)
  def setQuit(q: Char, d: String): Unit = {
    quit = Option(q)
    quitDescription = d
  }

  def get(msg: String, useQuit: Boolean, choices: Map[Char, String],
      defaultChoice: Option[Char] = None): Char = {
    val c = quit.map(q =>
      if (useQuit) {
        if (choices.contains(q)) error(s"quit option '$q' already in choices")
        choices.updated(q, quitDescription)
      } else choices
    ).getOrElse(choices)
    if (c.isEmpty) error("must specify at least one choice")
    val prompt = add(msg, choices, defaultChoice)
    os.print(prompt)
    os.flush()
    'a'
  }

  private def add(msg: String, choices: Map[Char, String], d: Option[Char]) = {
    val prompt = new StringBuilder(if (msg.isEmpty) "(" else " (")
    d match {
      case Some(s) =>
        if (!choices.contains(s)) error(s"default option '$s' not in choices")
        prompt ++= s") def '$s': "
      case _ => prompt ++= "): "
    }
    prompt.result()
  }

  private def error(msg: String) = throw DomainException(msg)
}

object Choice {
  val DefaultQuitDescription = "quit"

  def apply(): Choice = new Choice
  def apply(q: Char, d: String = DefaultQuitDescription): Choice =
    new Choice(quitIn = Option(q), quitDescription = d)

  private def checkChoice(c: Char, msg: String): Unit = {
    if (c < ' ' || c > '~')
      error(s"invalid $msg: 0x" + c.toInt.toHexString)
  }

  private def error(msg: String) = throw DomainException(msg)
}
