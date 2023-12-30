package quiz.kanji

import quiz.kanji.Ucd.LinkType.*
import quiz.kanji.Ucd.{LinkType, Sources}
import quiz.kanji.UcdTest.dog
import quiz.test.BaseTest
import quiz.test.BaseTest.{emptySources, testRadical}
import quiz.utils.Code

class UcdTest extends BaseTest:
  "name returns UTF16 value of code" in {
    val ucd = Ucd(dog, testRadical, 0, "", None, Nil, emptySources, Nil, NoLinkType, "", "")
    assert(ucd.name == "犬")
  }

  "oldLinks is Yes if linkType is Traditional or Traditional_R" in {
    values.foreach { lt =>
      val ucd = Ucd(dog, testRadical, 0, "", None, Nil, emptySources, Nil, lt, "", "")
      val expected = if lt == Traditional || lt == Traditional_R then OldLinks.Yes else OldLinks.No
      assert(ucd.oldLinks == expected)
    }
  }

  "linkedReadings is Yes if linkType ends in _R" in {
    values.foreach { lt =>
      val ucd = Ucd(dog, testRadical, 0, "", None, Nil, emptySources, Nil, lt, "", "")
      val expected = if lt.toString.endsWith("_R") then LinkedReadings.Yes else LinkedReadings.No
      assert(ucd.linkedReadings == expected)
    }
  }

  "create joyo source" in {
    val s = Sources("", "", true, false)
    assert(s.isJoyo)
    assert(!s.isJinmei)
    assert(s.toString.isEmpty)
  }

  "create jinmei source" in {
    val s = Sources("", "", false, true)
    assert(!s.isJoyo)
    assert(s.isJinmei)
    assert(s.toString.isEmpty)
  }

  "create Sources with valid region" in Seq("G", "H", "J", "K", "T", "V").foreach { x =>
    val s = Sources("", x, false, false)
    assert(!s.isJoyo)
    assert(!s.isJinmei)
    assert(s.toString == x)
  }

  "error for source with both joyo and jinmei" in {
    error(Sources("", "", true, true), "Sources can't be both joyo and jinmei")
  }

  "error if source region is not recognized" in {
    error(Sources("", "X", false, false), "Sources got unrecognized region 'X'")
  }

object UcdTest:
  private val dog = Code(0x72ac)
