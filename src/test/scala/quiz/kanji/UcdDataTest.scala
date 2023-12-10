package quiz.kanji

import quiz.kanji.RadicalData.Radical
import quiz.kanji.UcdData.{LinkType, UcdFileName}
import quiz.kanji.UcdDataTest.*
import quiz.utils.FileTest
import quiz.utils.UnicodeUtils.Code

import java.nio.file.{Files, Path}

class UcdDataTest extends FileTest {
  "file with just a header doesn't load any data" in { assert(create().size == 0) }

  "file with one row loads expected data" in {
    val data = create("72AC\t94\t4\tquǎn\t20234\t2868\tGHJKTV\tJ0-3824\tY\t\t\t\tdog\tケン いぬ")
    data.find("犬").map { ucd =>
      assert(ucd.code.value == 0x72ac)
      assert(ucd.radical == testRadical)
      assert(ucd.strokes == 4)
      assert(ucd.pinyin == "quǎn")
      assert(ucd.morohashiId.contains(MorohashiId(20234)))
      assert(ucd.nelsonIds == List(2868))
      assert(ucd.source.toString == "GHJKTV")
      assert(ucd.jSource == "J0-3824")
      assert(ucd.joyo)
      assert(!ucd.jinmei)
      assert(ucd.links.isEmpty)
      assert(!ucd.linkType.isDefined)
      assert(ucd.meaning == "dog")
      assert(ucd.reading == "ケン いぬ")
    }.orElse(fail("find failed"))
  }

  "file with two rows" in {
    val data = create("72AC\t94\t4" + "\t".repeat(11), "732B\t94\t11" + "\t".repeat(11))
    data.find("犬").map { ucd =>
      assert(ucd.code.value == 0x72ac)
      assert(ucd.strokes == 4)
    }.orElse(fail("find failed"))
    data.find("猫").map { ucd =>
      assert(ucd.code.value == 0x732b)
      assert(ucd.strokes == 11)
    }.orElse(fail("find failed"))
  }

  "rows with links" in {
    // sample Jinmei Kanji (9059) and another Kanji (48A3) that links back to it
    val data = create("9059\t162\t14\t\t\t\t\t\t\tY\t9065\tJinmei\t\t",
      "48A3\t162\t14\t\t\t\t\t\t\t\t9059\tDefinition*\t\t")
    data.find("遙").map { ucd =>
      assert(ucd.links == List(Code(0x9065)))
      assert(ucd.linkType == LinkType.Jinmei)
      assert(ucd.jinmei)
    }.orElse(fail("find failed"))
    data.find("䢣").map { ucd =>
      assert(ucd.links == List(Code(0x9059)))
      assert(ucd.linkType == LinkType.Definition_R)
      assert(!ucd.jinmei)
    }.orElse(fail("find failed"))
  }

  private def create(lines: String*) = {
    Files.writeString(
      tempDir.resolve(UcdFileName), if (lines.isEmpty) header else header + lines.mkString("\n"))
    UcdData(tempDir, testRadicalData)
  }
}

object UcdDataTest {
  private val testRadical = Radical(1, "一", Nil, "", "")
  private val testRadicalData = new RadicalData(Path.of("")) {
    override def findByNumber(i: Int): Radical = testRadical
  }
  private val header =
    "Code\tRadical\tStrokes\tPinyin\tMorohashiId\tNelsonIds\tSources\tJSource\tJoyo\tJinmei\t" +
      "LinkCodes\tLinkType\tMeaning\tJapanese\n"
}
