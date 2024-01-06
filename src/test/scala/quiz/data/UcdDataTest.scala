package quiz.data

import quiz.data.UcdData.{MaxNelsonId, UcdFileName}
import quiz.data.UcdDataTest.*
import quiz.kanji.Ucd.LinkType
import quiz.kanji.{MorohashiId, Radical}
import quiz.test.BaseTest.testRadical
import quiz.test.FileTest
import quiz.utils.Code

import java.nio.file.{Files, Path}

class UcdDataTest extends FileTest:
  private val dogFields = Vector("72AC", "94", "4", "quǎn", "20234", "2868", "GHJKTV", "J0-3824",
    "Y", "", "", "", "dog", "ケン いぬ")

  "file with just a header doesn't load any data" in { assert(create().size == 0) }

  "file with one row loads expected data" in {
    val data = create(dogFields.mkString("\t"))
    data.find("犬").map { ucd =>
      assert(ucd.code.value == 0x72ac)
      assert(ucd.name == ucd.code.toUTF16)
      assert(ucd.radical == testRadical)
      assert(ucd.strokes == dogFields(2).toInt)
      assert(ucd.pinyin == dogFields(3))
      assert(ucd.morohashiId.contains(MorohashiId(dogFields(4))))
      assert(ucd.nelsonIds == List(dogFields(5).toInt))
      assert(ucd.source.toString == dogFields(6))
      assert(ucd.jSource == dogFields(7))
      assert(ucd.joyo)
      assert(!ucd.jinmei)
      assert(ucd.links.isEmpty)
      assert(!ucd.linkType.isDefined)
      assert(ucd.meaning == dogFields(12))
      assert(ucd.reading == dogFields(13))
    }.orElse(fail("find failed"))
  }

  "file with two rows" in {
    val data = create(dogFields.mkString("\t"), "732B\t94\t11" + "\t".repeat(11))
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
    val link = Code(0x9065)
    data.find("遙").map { ucd =>
      assert(ucd.links == List(link))
      assert(ucd.linkType == LinkType.Jinmei)
      assert(ucd.jinmei)
      assert(ucd.linkedJinmei.contains(link))
    }.orElse(fail("find failed"))
    data.find("䢣").map { ucd =>
      assert(ucd.links == List(Code(0x9059)))
      assert(ucd.linkNames == List("遙"))
      assert(ucd.linkType == LinkType.Definition_R)
      assert(!ucd.jinmei)
      assert(ucd.linkedJinmei.isEmpty)
    }.orElse(fail("find failed"))
  }

  "file with Nelson Ids" in {
    val ids = List(1, MaxNelsonId)
    val data = create(dogFields.updated(5, ids.mkString(",")).mkString("\t"))
    data.find("犬").map(ucd => assert(ucd.nelsonIds == ids)).orElse(fail("find failed"))
  }

  "file with invalid Nelson Ids" in Seq("a", "-1", "0", "1,0", s"${MaxNelsonId + 1}").foreach(ids =>
    error(create(dogFields.updated(5, ids).mkString("\t")).find("犬"),
      _.contains(s"invalid NelsonIds '$ids'")))

  "file with invalid Link Codes" in Seq("z", "110000", "ABCD,-1").foreach(codes =>
    error(create(dogFields.updated(10, codes).mkString("\t")).find("犬"),
      _.contains(s"invalid LinkCodes '$codes'")))

  private def create(lines: String*) =
    Files.writeString(
      tempDir.resolve(UcdFileName), if lines.isEmpty then header else header + lines.mkString("\n"))
    UcdData(tempDir, testRadicalData)

object UcdDataTest:
  private val testRadicalData =
    new RadicalData(Path.of("")):
      override def findByNumber(i: Int): Radical = testRadical
  private val header =
    "Code\tRadical\tStrokes\tPinyin\tMorohashiId\tNelsonIds\tSources\tJSource\tJoyo\tJinmei\t" +
      "LinkCodes\tLinkType\tMeaning\tJapanese\n"
