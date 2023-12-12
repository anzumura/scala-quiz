package quiz.utils

import quiz.test.BaseTest
import quiz.utils.Block.isKanji

class BlockTest extends BaseTest {
  "create Unicode block with only one entry" in {
    val b = Block(Code(256))
    assert(b.start.value == 256)
    assert(b.start == b.end)
  }

  "block end cannot be less than start" in {
    error(Block(Code(20), Code(19)), "end U+0013 is less than start U+0014")
  }

  private val nonKanjiStrings = Seq("a", "ā", "あ", "ア")
  private val nonKanjiCodes = nonKanjiStrings.map(Code(_))

  // common Kanji from Unified block, Compatibility block and Extension B
  private val commonCodes = Seq("厭", "琢", "𠮟").map(Code(_))

  // rare Kanji from Radicals Supp. block
  private val rareCode = Code("⺠")

  "isCommonKanji tests if a Kanji is in one of the common blocks" in {
    commonCodes.foreach(c => assert(c.isCommonKanji))
    assert(!rareCode.isCommonKanji)
    nonKanjiCodes.foreach(c => assert(!c.isCommonKanji))
  }

  "isRareKanji tests if a Kanji is in one of the rare blocks" in {
    commonCodes.foreach(c => assert(!c.isRareKanji))
    assert(rareCode.isRareKanji)
    nonKanjiCodes.foreach(c => assert(!c.isCommonKanji))
  }

  "isKanji tests if a Kanji is in any Kanji block" in {
    commonCodes.foreach(c => assert(c.isKanji))
    assert(rareCode.isKanji)
    nonKanjiCodes.foreach(c => assert(!c.isKanji))
  }

  "isKanji helper method taking a string" in {
    val dog = Code(0x72ac)
    val scold = Code(0x20b9f)
    assert(isKanji(dog.toUTF16))
    assert(isKanji(scold.toUTF16))
    assert(isKanji(scold.toUTF16 + dog.toUTF16, sizeOne = false))
    nonKanjiStrings.foreach(c => assert(!isKanji(c)))
  }
}
