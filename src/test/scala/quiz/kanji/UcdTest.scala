package quiz.kanji

import quiz.kanji.Ucd.LinkType
import quiz.kanji.Ucd.LinkType.*
import quiz.kanji.UcdTest.dog
import quiz.test.BaseTest
import quiz.test.BaseTest.{emptySources, testRadical}
import quiz.utils.Code

class UcdTest extends BaseTest {
  "name returns UTF16 value of code" in {
    val ucd = Ucd(dog, testRadical, 0, "", None, Nil, emptySources, Nil, NoLinkType, "", "")
    assert(ucd.name == "犬")
  }

  "oldLinks is Yes if linkType is Traditional or Traditional_R" in {
    values.foreach { lt =>
      val ucd = Ucd(dog, testRadical, 0, "", None, Nil, emptySources, Nil, lt, "", "")
      val expected = if (lt == Traditional || lt == Traditional_R) OldLinks.Yes else OldLinks.No
      assert(ucd.oldLinks == expected)
    }
  }

  "linkedReadings is Yes if linkType ends in _R" in {
    values.foreach { lt =>
      val ucd = Ucd(dog, testRadical, 0, "", None, Nil, emptySources, Nil, lt, "", "")
      val expected = if (lt.toString.endsWith("_R")) LinkedReadings.Yes else LinkedReadings.No
      assert(ucd.linkedReadings == expected)
    }
  }
}

object UcdTest {
  private val dog = Code(0x72ac)
}
