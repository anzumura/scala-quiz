package quiz.kanji

import quiz.kanji.RadicalData.{MaxRadical, Radical, RadicalFileName}
import quiz.utils.FileTest

import java.nio.file.Files

class RadicalDataTest extends FileTest {
  private val sampleRadicals = "1\t一\t一部（いちぶ）\tイチ\n2\t丨\t丨部（こんぶ）\tぼう たてぼう"
  private val firstRadical = Radical(1, "一", Nil, "一部（いちぶ）", "イチ")
  private val secondRadical = Radical(2, "丨", Nil, "丨部（こんぶ）", "ぼう たてぼう")

  "find by name returns expected Radicals" in {
    val data = create(true, sampleRadicals)
    assert(data.findByName("一").contains(firstRadical))
    assert(data.findByName("丨").contains(secondRadical))
    assert(data.findByName("二").isEmpty)
  }

  "find by number returns expected Radicals" in {
    val data = create(true, sampleRadicals)
    assert(data.findByNumber(1) == firstRadical)
    assert(data.findByNumber(2) == secondRadical)
  }

  "load a radical with altNames" in {
    val data = create(true, "9\t人 亻\t人部（じんぶ）\tひと にんべん ひとがしら ひとやね")
    val result = data.findByNumber(9)
    assert(result.name == "人")
    assert(result.altNames == List("亻"))
  }

  "find by number out of range" in {
    val data = create(true, sampleRadicals)
    Seq(-1, 0, MaxRadical + 1).foreach(i =>
      error(data.findByNumber(i), s"invalid Radical number '$i' (must be between 1 and 214)"))
  }

  "Radical number can't be zero" in {
    error(create("0\t一\t\t").findByName("一"), _.contains("Radical number can't be zero"))
  }

  "Radical number can't be bigger than 214" in {
    error(create("215\t一\t\t").findByName("一"), _.contains("exceeded max value 214"))
  }

  "duplicate Radical number" in {
    error(
      create("1\t一\t\t", "1\t二\t\t").findByName("一"), _.contains("duplicate Radical number '1'"))
  }

  "Radical name can't be empty" in {
    error(create("1\t\t\t").findByName("一"), _.contains("Radical name can't be empty"))
  }

  "file must contain 214 Radicals" in {
    domainError(create("1\t一\t\t").findByName("一"), "loaded 1, but expected 214")
  }

  private def create(skipVerifyLoaded: Boolean, lines: String*): RadicalData = {
    Files.writeString(tempDir.resolve(RadicalFileName),
      if (lines.isEmpty) header else header + lines.mkString("\n"))
    if (skipVerifyLoaded) new RadicalData(tempDir) {
      override def verifyDataSize(x: Int): Unit = {}
    }
    else RadicalData(tempDir)
  }

  private def create(lines: String*): RadicalData = create(false, lines: _*)

  private val header = "Number\tName\tLongName\tReading\n"
}
