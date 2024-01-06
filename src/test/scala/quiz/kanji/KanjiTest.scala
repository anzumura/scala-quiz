package quiz.kanji

import quiz.kanji.JinmeiReason.Print
import quiz.kanji.Kanji.Info
import quiz.kanji.Kanji.Info.Frequency
import quiz.kanji.Ucd.LinkType.NoLinkType
import quiz.kanji.Ucd.{LinkType, Sources}
import quiz.test.BaseTest
import quiz.test.BaseTest.emptySources
import quiz.utils.Code

import scala.language.implicitConversions

class KanjiTest extends BaseTest:
  import KanjiTest.*

  "Numbered Kanji" - {
    "create Jouyou Kanji with valid grade" in Grade.values.filter(_ != Grade.NoGrade).foreach { g =>
      val k = JouyouKanji(params, radicalName, strokes, meaning, reading, number, year, oldNames, g)
      checkOfficialFields(k)
      assert(k.kanjiType == KanjiType.Jouyou)
      assert(k.suffix == '.')
      assert(k.grade == g)
      assert(k.oldNames == oldNames)
      assert(!k.hasReason)
    }

    "info method returns expected string based on 'exclude' value" in {
      val k = JouyouKanji(
        params, radicalName, strokes, meaning, reading, number, year, oldNames, Grade.G1)
      val prefix = s"$k${k.suffix} Rad ${k.radical}, Strokes ${k.strokes}"
      val frq = s", Frq ${k.frequency}"
      assert(k.info() == s"$prefix$frq, ${k.grade}" + s", ${k.level}" + s", ${k.kyu}")
      assert(k.info(Info.Frequency) == s"$prefix, ${k.grade}" + s", ${k.level}" + s", ${k.kyu}")
      assert(k.info(Info.Grade) == s"$prefix$frq, ${k.level}" + s", ${k.kyu}")
      assert(k.info(Info.Level) == s"$prefix$frq, ${k.grade}" + s", ${k.kyu}")
      assert(k.info(Info.Kyu) == s"$prefix$frq, ${k.grade}" + s", ${k.level}")
    }

    "equals is based on name" in {
      // fake examples, real data loaded from files has unique names per Kanji
      val k1 = JouyouKanji(
        params, radicalName, strokes, meaning, reading, number, year, oldNames, Grade.G1)
      val k2 = JouyouKanji(
        params, radicalName, strokes, meaning, reading, number, year, oldNames, Grade.G2)
      val k3 = JouyouKanji(
        data.params("違"), radicalName, strokes, meaning, reading, number, year, oldNames, Grade.G1)
      assert(k1 == k2)
      assert(k1 != k3)
      val x: Any = k1.name
      assert(k1 != x)
    }

    "error for Jouyou Kanji with no grade" in {
      error(JouyouKanji(params, radicalName, strokes, meaning, reading, number, year, oldNames,
          Grade.NoGrade), "JouyouKanji: must have a valid grade")
    }

    "create Jinmei Kanji with valid reason" in
      JinmeiReason.values.filter(_ != JinmeiReason.NoJinmeiReason).foreach { reason =>
        val k = JinmeiKanji(params, radicalName, reading, number, year, oldNames, reason)
        checkOfficialFields(k)
        assert(k.kanjiType == KanjiType.Jinmei)
        assert(k.oldNames == oldNames)
        assert(k.reason == reason)
        assert(!k.hasGrade)
      }

    "Jinmei Kanji suffix depends on JLPT Level" in Level.values.foreach { l =>
      val d: BaseKanjiData =
        new TestKanjiData:
          override def level(s: String): Level = l
      val k = JinmeiKanji(d.params(name), radicalName, reading, number, year, oldNames, Print)
      assert(k.suffix == (if l.isDefined then '\'' else '^'))
    }

    "error for Jinmei Kanji with no reason" in {
      error(JinmeiKanji(params, radicalName, reading, number, year, oldNames,
          JinmeiReason.NoJinmeiReason), "JinmeiKanji: must have a valid reason")
    }

    "error for Jinmei Kanji with no year" in {
      error(JinmeiKanji(params, radicalName, reading, number, 0, oldNames, JinmeiReason.Names),
        "JinmeiKanji: must have a valid year")
    }

    "create Extra Kanji with no new name" in {
      val k = ExtraKanji(params, radicalName, strokes, meaning, reading, number)
      checkNumberedFields(k)
      assert(k.kanjiType == KanjiType.Extra)
      assert(k.suffix == '+')
      assert(!k.hasNewName)
    }

    "error for Kanji with number <= 0" in Seq(-1, 0).foreach(num =>
      error(ExtraKanji(params, radicalName, strokes, meaning, reading, num),
        "ExtraKanji: number must be greater than zero"))
  }

  "Linked Kanji" - {
    "create Linked Jinmei Kanji" in {
      val link = JouyouKanji(
        params, radicalName, strokes, meaning, reading, number, year, oldNames, Grade.G2)
      val d: BaseKanjiData =
        new TestKanjiData:
          override def frequency(s: String): Int = differentFrequency
          override def kyu(s: String): Kyu = differentKyu
      val k = LinkedJinmeiKanji(d.params(name), link)
      checkLinkedFields(k, link)
      assert(k.kanjiType == KanjiType.LinkedJinmei)
      assert(k.suffix == '~')
    }

    "create Linked Old Kanji" in {
      val link = JouyouKanji(
        params, radicalName, strokes, meaning, reading, number, year, oldNames, Grade.G2)
      val d: BaseKanjiData =
        new TestKanjiData:
          override def frequency(s: String): Int = differentFrequency
          override def kyu(s: String): Kyu = differentKyu
      val k = LinkedOldKanji(d.params(name), link)
      checkLinkedFields(k, link)
      assert(k.kanjiType == KanjiType.LinkedOld)
      assert(k.suffix == '%')
    }

    "error for Linked Jinmei Kanji with non-official link" in {
      val link = ExtraKanji(params, radicalName, strokes, meaning, reading, number)
      error(LinkedJinmeiKanji(params, link),
        "LinkedJinmeiKanji: link must be JouyouKanji or JinmeiKanji")
    }

    "error for Linked Old Kanji with non-Jouyou link" in {
      val link = ExtraKanji(params, radicalName, strokes, meaning, reading, number)
      error(LinkedOldKanji(params, link), "LinkedOldKanji: link must be JouyouKanji")
    }
  }

  "Other Kanji" - {
    "create Frequency Kanji" in {
      val k = FrequencyKanji(params, differentFrequency)
      checkLoadedFields(k)
      assert(k.kanjiType == KanjiType.Frequency)
      assert(k.suffix == '"')
      assert(k.kyu == kyu)
      assert(k.frequency == differentFrequency)
    }

    "error for Frequency Kanji with no frequency" in Seq(-1, 0).foreach(f =>
      error(FrequencyKanji(params, f), "FrequencyKanji: frequency must be greater than zero"))

    "create Kentei Kanji" in {
      val k = KenteiKanji(params, differentKyu)
      checkLoadedFields(k)
      assert(k.kanjiType == KanjiType.Kentei)
      assert(k.kyu == differentKyu)
      assert(!k.hasFrequency)
    }

    "Kentei suffix depends on kyu" in Kyu.defined.foreach { x =>
      val k = KenteiKanji(params, x)
      assert(k.suffix == (if x == Kyu.K1 then '#' else '@'))
    }

    "error for Kentei Kanji with no kyu" in {
      error(KenteiKanji(params, Kyu.NoKyu), "KenteiKanji: must have a valid kyu")
    }

    "create Ucd Kanji" in {
      val k = UcdKanji(params)
      checkLoadedFields(k)
      assert(k.kanjiType == KanjiType.Ucd)
      assert(k.suffix == '*')
      assert(!k.hasKyu)
      assert(!k.hasFrequency)
    }

    "create Ucd Kanji with old names" in {
      val oldNames = List("辨", "瓣", "辯")
      val d: BaseKanjiData =
        new TestKanjiData:
          override def getUcd(s: String): Ucd = testUcd
            .copy(linkType = LinkType.Traditional, links = oldNames.map(Code(_)))
      val k = UcdKanji(d.params(name))
      checkLoadedFields(k)
      assert(k.oldNames == oldNames)
      assert(!k.hasNewName)
    }

    "create Ucd Kanji with new name" in {
      val newName = "弁"
      val d: BaseKanjiData =
        new TestKanjiData:
          override def getUcd(s: String): Ucd = testUcd
            .copy(linkType = LinkType.Simplified, links = List(Code(newName)))
      val k = UcdKanji(d.params(name))
      checkLoadedFields(k)
      assert(!k.hasOldNames)
      assert(k.newName.contains(newName))
    }
  }

  private def checkFields(k: Kanji) =
    assert(k.name == name)
    assert(k.radical == radical)
    assert(k.strokes == strokes)

  private def checkLinkedFields(k: Kanji, link: Kanji) =
    assert(k.link.contains(link))
    assert(k.newName.contains(link.name))
    assert(k.meaning == link.meaning)
    assert(k.reading == link.reading)
    assert(k.linkedReadings)
    assert(k.frequency == differentFrequency)
    assert(k.kyu == differentKyu)
    // these should all have default values for linked (ie non-Loaded) Kanji
    assert(k.grade == Grade.NoGrade)
    assert(!k.hasLevel)
    assert(!k.hasReason)
    assert(!k.hasNumber)
    assert(!k.hasYear)
    assert(!k.hasOldNames)

  private def checkLoadedFields(k: Kanji) =
    assert(k.meaning == meaning)
    assert(k.reading == reading)
    assert(!k.hasLink)
    checkFields(k)

  private def checkNumberedFields(k: Kanji) =
    assert(k.kyu == kyu)
    assert(k.number == number)
    assert(!k.linkedReadings)
    checkLoadedFields(k)

  private def checkOfficialFields(k: Kanji) =
    assert(k.level == level)
    assert(k.frequency == frequency)
    assert(k.year == year)
    assert(!k.hasNewName)
    checkNumberedFields(k)

object KanjiTest:
  // sample Kanji fields
  private val name = "海"
  private val radicalName = "水"
  private val radical = Radical(85, "水", List("氵", "氺"), "水部（すいぶ）", "みず さんずい したみず")
  private val strokes = 9
  private val meaning = "sea"
  private val reading = "カイ、うみ"
  private val oldNames = List("亙")
  private val kyu = Kyu.K9
  private val differentKyu = Kyu.KJ1
  private val number = 182
  private val level = Level.N4
  private val frequency = 200
  private val differentFrequency = 2489
  private val year = 2023
  private val testUcd = Ucd(Code(1), radical, strokes, "", None, Nil, emptySources, Nil,
    LinkType.NoLinkType, meaning, reading)

  class TestKanjiData extends BaseKanjiData:
    override def level(s: String): Level = KanjiTest.level
    override def kyu(s: String): Kyu = KanjiTest.kyu
    override def frequency(s: String): Int = KanjiTest.frequency
    def getUcd(s: String): Ucd = testUcd
    def getRadical(s: String): Radical = radical
  private val data = TestKanjiData()
  private val params = data.params(name)
