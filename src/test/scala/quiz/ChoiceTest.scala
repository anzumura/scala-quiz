package quiz

import org.scalatest.freespec.AnyFreeSpec
import quiz.Choice.DefaultQuitDescription

class ChoiceTest extends AnyFreeSpec {
  "create with default options" in {
    val c = Choice()
    assert(c.quit.isEmpty)
    assert(DefaultQuitDescription == c.quitDescription)
  }

  "create with quit option" in {
    val expected = 'q'
    val c = Choice(expected)
    assert(c.isQuit(expected))
    assert(DefaultQuitDescription == c.quitDescription)
  }

  "create with quit option and description" in {
    val expected = 'q'
    val expectedDesc = "my quit"
    val c = Choice(expected, expectedDesc)
    assert(c.isQuit(expected))
    assert(expectedDesc == c.quitDescription)
  }

  "create with invalid quit option" in {
    val e = intercept[DomainException] { Choice(12) }
    assert("invalid quit option: 0xc" == e.getMessage)
  }

  "update quit value" in {
    val expected = 'q'
    val c = Choice()
    assert(!c.hasQuit)
    c.setQuit(expected)
    assert(c.hasQuit && c.quit.contains(expected))
    c.clearQuit()
    assert(!c.hasQuit && c.quit.isEmpty)
  }

  "update to invalid quit option" in {
    val c = Choice()
    val e = intercept[DomainException] { c.quit = Option(13) }
    assert("invalid quit option: 0xd" == e.getMessage)
  }
}
