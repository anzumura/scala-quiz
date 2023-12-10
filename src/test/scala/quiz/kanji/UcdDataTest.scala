package quiz.kanji

import quiz.kanji.UcdData.UcdFileName
import quiz.utils.FileTest

import java.nio.file.Files

class UcdDataTest extends FileTest {
  "file with just a header doesn't load any data" in { assert(create().size == 0) }

  "file with one row loads expected data" in {
    val data = create("72AC\t94\t4\tquǎn\t20234\t2868\tGHJKTV\tJ0-3824\tY\t\t\t\tdog\tケン いぬ")
    data.find("犬").map { ucd =>
      assert(ucd.code.value == 0x72ac)
      assert(ucd.radical == "94")
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

  private def create(lines: String*) = {
    Files.writeString(
      tempDir.resolve(UcdFileName), if (lines.isEmpty) header else header + lines.mkString("\n"))
    UcdData(tempDir)
  }

  private val header =
    "Code\tRadical\tStrokes\tPinyin\tMorohashiId\tNelsonIds\tSources\tJSource\tJoyo\tJinmei\t" +
      "LinkCodes\tLinkType\tMeaning\tJapanese\n"
}
