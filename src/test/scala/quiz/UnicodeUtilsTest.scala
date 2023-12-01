package quiz

import UnicodeUtils._

class UnicodeUtilsTest extends BaseTest {
  private val dog = Code(0x72ac) // Kanji in the Basic Multilingual Plane
  private val scold = Code(0x20b9f) // Kanji in a Supplementary Plane

  "code" - {
    "toString returns the standard Unicode representation" in {
      assert(Code().toString == "U+0000")
      assert(Code(62).toString == "U+003E")
      assert(Code(0xfff).toString == "U+0FFF")
      assert(Code(0xffff).toString == "U+FFFF")
      assert(Code(0x10000).toString == "U+10000")
      assert(Code(UnicodeMax).toString == "U+10FFFF")
    }

    "toUTF16 returns the code point as a standard Java (UTF-16) String" in {
      val result = dog.toUTF16
      assert(result == "犬")
      assert(result.length == 1)
    }

    "toUTF16 works for codes outside of BMP (Basic Multilingual Plane)" in {
      val result = scold.toUTF16
      assert(result == "𠮟")
      assert(result.length == 2)
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
      assert(Code("犬").value == dog.value)
      assert(Code("𠮟").value == scold.value)
    }

    "create from longer string value" in {
      assert(Code("犬猫", sizeOne = false).value == 0x72ac)
    }

    "can't create from empty string" in {
      domainException(Code(""), "cannot create Unicode Code from empty string")
    }

    "can't create from longer string is sizeOne is true (the default)" in {
      domainException(Code("犬猫"), "'犬猫' has more than one Unicode letter")
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

  private val nonKanjiStrings = Seq("a", "ā", "あ", "ア")
  private val nonKanjiCodes = nonKanjiStrings.map(Code(_))

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

  "isKanji helper method taking a string" in {
    assert(isKanji(dog.toUTF16))
    assert(isKanji(scold.toUTF16))
    assert(isKanji(scold.toUTF16 + dog.toUTF16, sizeOne = false))
    nonKanjiStrings.foreach(c => assert(!isKanji(c)))
  }
}
