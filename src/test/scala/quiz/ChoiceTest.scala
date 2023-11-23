package quiz

import org.scalatest.freespec.AnyFreeSpec
import quiz.Choice._

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, PrintStream}

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
    val e = intercept[DomainException] { Choice(12: Char) }
    assert("invalid quit option: '0xc'" == e.getMessage)
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
    assert("invalid quit option: '0xd'" == e.getMessage)
  }

  "update quit and quit description" in {
    val expected = 'q'
    val expectedDesc = "my quit"
    val c = Choice()
    c.setQuit(expected, expectedDesc)
    assert(c.isQuit(expected))
    assert(expectedDesc == c.quitDescription)
  }

  "get" - {
    "one choice" in {
      val (c, os) = create("a")
      val msg = "my choice"
      assert('a' == c.get(msg, Choices(('a', "first")), QuitOn))
      assert(s"$msg (a=first): " == os.toString)
    }

    "choice with no message" in {
      val (c, os) = create("a")
      assert('a' == c.get(Map(('a', ""), ('b', "second"))))
      assert("(a, b=second): " == os.toString)
    }

    "quit choice" in {
      val expected = 'q'
      val (c, os) = create(s"$expected")
      c.setQuit(expected)
      assert(expected == c.get(Map()))
      assert(s"($expected=quit): " == os.toString)
    }

    "choice with no description" in {
      val (c, os) = create("a")
      assert('a' == c.get(Map(('a', ""), ('b', "second"))))
      assert("(a, b=second): " == os.toString)
    }

    "multiple choices are sorted" in {
      val (c, os) = create("a")
      c.get(Map(('c', "third"), ('b', "second"), ('a', "first")))
      assert("(a=first, b=second, c=third): " == os.toString)
    }

    "multiple consecutive choices without descriptions use '-'" in {
      val (c, os) = create("e")
      c.get(Map(('e', "E"), ('f', ""), ('g', ""), ('h', ""), ('j', "")))
      assert("(e=E, f-h, j): " == os.toString)
    }

    "with default" in {
      val (c, os) = create("a")
      val msg = "my default"
      assert('a' == c.get(msg, Map(('a', "first")), 'a'))
      assert(s"$msg (a=first) def 'a': " == os.toString)
    }

    "empty input selects default" in {
      val (c, os) = create()
      assert('b' == c.get(Map(('a', "first"), ('b', "second")), 'b'))
      assert("(a=first, b=second) def 'b': " == os.toString)
    }

    "quit can be the default" in {
      val (c, os) = create("x")
      c.setQuit('x', "my quit")
      assert('x' == c.get(Map(('a', "first")), 'x'))
      assert("(a=first, x=my quit) def 'x': " == os.toString)
    }

    "suppress 'quit' option" in {
      val (c, os) = create("a")
      c.setQuit('q')
      assert('a' == c.get("my msg", Map(('a', "")), QuitOff))
      assert("my msg (a): " == os.toString())
    }

    "suppress 'quit' option and include same choice" in {
      val (c, os) = create("q")
      c.setQuit('q')
      assert('q' == c.get(Map(('q', "Q")), QuitOff))
      assert("(q=Q): " == os.toString())
    }

    "suppress 'quit' option with default" in {
      val (c, os) = create()
      c.setQuit('q')
      assert('a' == c.get(Map(('a', "")), 'a', QuitOff))
      assert("(a) def 'a': " == os.toString())
    }
  }

  "get errors" - {
    "no choices specified" in {
      val c = Choice()
      val e = intercept[DomainException] { c.get(Map()) }
      assert("must specify at least one choice" == e.getMessage)
    }

    "invalid choice" in {
      val c = Choice()
      val e = intercept[DomainException] { c.get(Map(('a', "A"), (14, "bad"))) }
      assert("invalid option: '0xe'" == e.getMessage)
    }

    "default option not in choices" in {
      val c = Choice()
      val e = intercept[DomainException] { c.get(Map(('a', "")), 'b') }
      assert("default option 'b' not in choices" == e.getMessage)
    }

    "quit option in choices" in {
      val c = Choice()
      c.setQuit('a')
      val e = intercept[DomainException] { c.get(Map(('a', ""))) }
      assert("quit option 'a' already in choices" == e.getMessage)
    }
  }

  private def create(input: String = "") = {
    val is = new ByteArrayInputStream((input + '\n').getBytes())
    val os = new ByteArrayOutputStream()
    (Choice(is, new PrintStream(os)), os)
  }
}
