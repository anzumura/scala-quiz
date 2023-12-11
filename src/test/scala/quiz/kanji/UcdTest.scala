package quiz.kanji

import quiz.kanji.RadicalData.Radical
import quiz.kanji.UcdData.{LinkType, Sources, Ucd}
import quiz.kanji.UcdTest.dog
import quiz.test.BaseTest
import quiz.test.BaseTest.{emptySources, testRadical}
import quiz.utils.UnicodeUtils.Code

class UcdTest extends BaseTest {
  "name returns UTF16 value of code" in {
    val ucd = Ucd(dog, testRadical, 0, "", None, Nil, emptySources, Nil, LinkType.None, "", "")
    assert(ucd.name == "犬")
  }
}

object UcdTest {
  private val dog = Code(0x72ac)
}
