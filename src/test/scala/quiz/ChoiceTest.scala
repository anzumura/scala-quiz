package quiz

import quiz.Choice._

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, PrintStream}

class ChoiceTest extends BaseTest {
  "create with default options" in {
    val c = Choice()
    assert(c.quit.isEmpty)
    assert(c.quitDescription == DefaultQuitDescription)
  }

  "create with quit option" in {
    val expected = 'q'
    val c = Choice(expected)
    assert(c.isQuit(expected))
    assert(c.quitDescription == DefaultQuitDescription)
  }

  "create with quit option and description" in {
    val expected = 'q'
    val expectedDesc = "my quit"
    val c = Choice(expected, expectedDesc)
    assert(c.isQuit(expected))
    assert(c.quitDescription == expectedDesc)
  }

  "create with invalid quit option" in {
    domainException(Choice(12: Char), "invalid quit option: '0xc'")
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
    domainException(c.quit = Option(13), "invalid quit option: '0xd'")
  }

  "update quit and quit description" in {
    val expected = 'q'
    val expectedDesc = "my quit"
    val c = Choice()
    c.setQuit(expected, expectedDesc)
    assert(c.isQuit(expected))
    assert(c.quitDescription == expectedDesc)
  }

  "get" - {
    "one choice" in {
      val (c, os) = create("a")
      val msg = "my choice"
      assert(c.get(Choices(('a', "first")), msg, QuitOn) == 'a')
      assert(os.toString == s"$msg (a=first): ")
    }

    "choice with no message" in {
      val (c, os) = create("a")
      assert(c.get(Map(('a', ""), ('b', "second"))) == 'a')
      assert(os.toString == "(a, b=second): ")
    }

    "quit choice" in {
      val expected = 'q'
      val (c, os) = create(s"$expected")
      c.setQuit(expected)
      assert(c.get(Map()) == expected)
      assert(os.toString == s"($expected=quit): ")
    }

    "choice with no description" in {
      val (c, os) = create("a")
      assert(c.get(Map(('a', ""), ('b', "second"))) == 'a')
      assert(os.toString == "(a, b=second): ")
    }

    "multiple choices are sorted" in {
      val (c, os) = create("a")
      c.get(Map(('c', "third"), ('b', "second"), ('a', "first")))
      assert(os.toString == "(a=first, b=second, c=third): ")
    }

    "multiple consecutive choices without descriptions use '-'" in {
      val (c, os) = create("e")
      c.get(Map(('e', "E"), ('f', ""), ('g', ""), ('h', ""), ('j', "")))
      assert(os.toString == "(e=E, f-h, j): ")
    }

    "with default" in {
      val (c, os) = create("a")
      val msg = "my default"
      assert(c.get(Map(('a', "first")), msg, 'a') == 'a')
      assert(os.toString == s"$msg (a=first) def 'a': ")
    }

    "empty input selects default" in {
      val (c, os) = create()
      assert(c.get(Map(('a', "first"), ('b', "second")), 'b') == 'b')
      assert(os.toString == "(a=first, b=second) def 'b': ")
    }

    "quit can be the default" in {
      val (c, os) = create("x")
      c.setQuit('x', "my quit")
      assert(c.get(Map(('a', "first")), 'x') == 'x')
      assert(os.toString == "(a=first, x=my quit) def 'x': ")
    }

    "suppress 'quit' option" in {
      val (c, os) = create("a")
      c.setQuit('q')
      assert(c.get(Map(('a', "")), "my msg", QuitOff) == 'a')
      assert(os.toString == "my msg (a): ")
    }

    "suppress 'quit' option and include same choice" in {
      val (c, os) = create("q")
      c.setQuit('q')
      assert(c.get(Map(('q', "Q")), QuitOff) == 'q')
      assert(os.toString == "(q=Q): ")
    }

    "suppress 'quit' option with default" in {
      val (c, os) = create()
      c.setQuit('q')
      assert(c.get(Map(('a', "")), 'a', QuitOff) == 'a')
      assert(os.toString == "(a) def 'a': ")
    }

    "invalid options prompt again" in {
      // input has 3 invalid options:
      // 1: 'z' is not included in Choices
      // 2: 'xx' is not a single character
      // 3: empty line (invalid when no default option is set)
      val (c, os) = create("z\nxx\n\n1")
      assert(c.get(Map('1' -> "")) == '1')
      // expect 4 prompts (3 bad options followed by a good one)
      assert(os.toString == "(1): ".repeat(4))
    }
  }

  "get errors" - {
    "no choices specified" in {
      val c = Choice()
      domainException(c.get(Map()), "must specify at least one choice")
    }

    "invalid choice" in {
      val c = Choice()
      domainException(c.get(Map(('a', "A"), (14, "bad"))),
        "invalid option: '0xe'")
    }

    "default option not in choices" in {
      val c = Choice()
      domainException(c.get(Map(('a', "")), 'b'),
        "default option 'b' not in choices")
    }

    "quit option in choices" in {
      val c = Choice()
      c.setQuit('a')
      domainException(c.get(Map(('a', ""))),
        "quit option 'a' already in choices")
    }
  }

  "get range" - {
    val a2c = Range('a', 'c')

    "is inclusive" in {
      val (c, os) = create("b")
      assert(c.get(a2c) == 'b')
      assert(os.toString == "(a-c): ")
    }

    "can have a single value" in {
      val (c, os) = create("x")
      val r = Range('x', 'x')
      assert(c.get(r) == 'x')
      assert(os.toString == "(x): ")
    }

    "supports quit option" in {
      val (c, os) = create("c")
      c.setQuit('q')
      assert(c.get(a2c) == 'c')
      assert(os.toString == "(a-c, q=quit): ")
    }

    "supports default option" in {
      val (c, os) = create()
      assert(c.get(a2c, 'c') == 'c')
      assert(os.toString == "(a-c) def 'c': ")
    }

    "supports suppressing quit option" in {
      val (c, os) = create()
      c.setQuit('q')
      assert(c.get(a2c, 'c', QuitOff) == 'c')
      assert(os.toString == "(a-c) def 'c': ")
    }

    "methods to merge with Choices" - {
      val choices = Choices('f' -> "foo", 'g' -> "")
      val msg = "msg"
      val prompt = "(a-c, f=foo, g"
      val msgPrompt = s"$msg $prompt"

      "simple case" in {
        val (c, os) = create("b\nb")
        c.setQuit('q')
        assert(a2c.get(c, choices) == 'b')
        assert(os.toString == prompt + ", q=quit): ")
        os.reset()
        assert(a2c.get(c, choices, msg) == 'b')
        assert(os.toString == msgPrompt + ", q=quit): ")
      }

      "suppress quit" in {
        val (c, os) = create("b\nb")
        c.setQuit('q')
        assert(a2c.get(c, choices, QuitOff) == 'b')
        assert(os.toString == prompt + "): ")
        os.reset()
        assert(a2c.get(c, choices, msg, QuitOff) == 'b')
        assert(os.toString == msgPrompt + "): ")
      }

      "default option" in {
        val (c, os) = create("\n")
        assert(a2c.get(c, choices, 'a') == 'a')
        assert(os.toString == prompt + ") def 'a': ")
        os.reset()
        assert(a2c.get(c, choices, msg, 'a') == 'a')
        assert(os.toString == msgPrompt + ") def 'a': ")
      }

      "default option and suppress quit" in {
        val (c, os) = create("\n")
        c.setQuit('b')
        assert(a2c.get(c, choices, 'a', QuitOff) == 'a')
        assert(os.toString == prompt + ") def 'a': ")
        os.reset()
        assert(a2c.get(c, choices, msg, 'a', QuitOff) == 'a')
        assert(os.toString == msgPrompt + ") def 'a': ")
      }
    }
  }

  "get range errors" - {
    "invalid range start" in {
      domainException(Range(15, 'c'), "invalid range start: '0xf'")
    }

    "invalid range end" in {
      domainException(Range('c', 10), "invalid range end: '0xa'")
    }

    "start greater than end" in {
      domainException(Range('c', 'b'), "start 'c' is greater than end 'b'")
    }

    "range overlaps with Choices" in {
      val c = Choice()
      val overlap = 'd'
      domainException(Range('a', 'f').get(c, Map(overlap -> "")),
        s"range option '$overlap' already in choices")
    }
  }

  private def create(input: String = "") = {
    val is = new ByteArrayInputStream((input + '\n').getBytes())
    val os = new ByteArrayOutputStream()
    (Choice(is, new PrintStream(os)), os)
  }
}
