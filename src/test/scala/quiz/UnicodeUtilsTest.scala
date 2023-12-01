package quiz

import UnicodeUtils._

class UnicodeUtilsTest extends BaseTest {
  "code" - {
    "toString" in {
      assert(Code().toString == "U+0000")
      assert(Code(62).toString == "U+003E")
      assert(Code(0xfff).toString == "U+0FFF")
      assert(Code(0xffff).toString == "U+FFFF")
      assert(Code(0x10000).toString == "U+10000")
      assert(Code(UnicodeMax).toString == "U+10FFFF")
    }

    "can't be negative" in {
      domainException(Code(-1), "code can't be negative")
    }

    "exceeds Unicode max U+10FFFF" in {
      val x = 0x110000
      domainException(Code(x),
        "code 0x%x exceeds Unicode max U+10FFFF".format(x))
    }

    "create from string value" in {
      assert(Code("犬").value == 0x72ac)
    }

    "create from longer string value" in {
      assert(Code("犬猫", sizeOne = false).value == 0x72ac)
    }

    "can't create from empty string" in {
      domainException(Code(""), "cannot create Unicode Code from empty string")
    }

    "can't create from longer string is sizeOne is true (the default)" in {
      domainException(Code("犬猫"), "value has more than one Unicode letter")
    }
  }

  "create Unicode block" in {
    val b = Block(Code(0), Code(255))
    assert(b.start.value == 0)
    assert(b.end.value == 255)
  }

  "create Unicode block with only one entry" in {
    val b = Block(Code(256))
    assert(b.start.value == 256)
    assert(b.start == b.end)
  }

  "block end cannot be less than start" in {
    domainException(Block(Code(20), Code(19)),
      "end U+0013 is less than start U+0014")
  }

  private val nonKanjiCodes = Seq("a", "ā", "あ", "ア").map(Code(_))

  // common Kanji from Unified block, Compatibility block and Extension B
  private val commonCodes = Seq("厭", "琢", "𠮟").map(Code(_))

  // rare Kanji from Radicals Supp. block
  private val rareCode = Code("⺠")

  "isCommonKanji tests if a Kanji is in one of the common blocks" in {
    commonCodes.foreach(c => assert(isCommonKanji(c)))
    assert(!isCommonKanji(rareCode))
    nonKanjiCodes.foreach(c => assert(!isCommonKanji(c)))
  }

  "isRareKanji tests if a Kanji is in one of the rare blocks" in {
    commonCodes.foreach(c => assert(!isRareKanji(c)))
    assert(isRareKanji(rareCode))
    nonKanjiCodes.foreach(c => assert(!isCommonKanji(c)))
  }

  "isKanji tests if a Kanji is in any Kanji block" in {
    commonCodes.foreach(c => assert(isKanji(c)))
    assert(isKanji(rareCode))
    nonKanjiCodes.foreach(c => assert(!isKanji(c)))
  }
}
