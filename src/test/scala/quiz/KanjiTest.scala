package quiz

class KanjiTest extends BaseTest {
  import KanjiTest._

  "Numbered Kanji" - {
    "create Jouyou Kanji with valid grade" in {
      Grade.values.filter(_ != Grade.None).foreach { grade =>
        val k = JouyouKanji(name, radical, strokes, meaning, reading, kyu,
          number, level, frequency, year, grade)
        checkOfficialFields(k)
        assert(k.kanjiType == KanjiType.Jouyou)
        assert(k.grade == grade)
        assert(k.reason == JinmeiReason.None)
      }
    }

    "error for Jouyou Kanji with no grade" in {
      domainException(JouyouKanji(name, radical, strokes, meaning, reading, kyu,
          number, level, frequency, year, Grade.None),
        "JouyouKanji: must have a valid grade")
    }

    "create Jinmei Kanji with valid reason" in {
      JinmeiReason.values.filter(_ != JinmeiReason.None).foreach { reason =>
        val k = JinmeiKanji(name, radical, strokes, meaning, reading, kyu,
          number, level, frequency, year, reason)
        checkOfficialFields(k)
        assert(k.kanjiType == KanjiType.Jinmei)
        assert(k.reason == reason)
        assert(k.grade == Grade.None)
      }
    }

    "error for Jinmei Kanji with no reason" in {
      domainException(JinmeiKanji(name, radical, strokes, meaning, reading, kyu,
          number, level, frequency, year, JinmeiReason.None),
        "JinmeiKanji: must have a valid reason")
    }

    "error for Jinmei Kanji with no year" in {
      domainException(JinmeiKanji(name, radical, strokes, meaning, reading, kyu,
          number, level, frequency, 0, JinmeiReason.Names),
        "JinmeiKanji: must have a valid year")
    }

    "create Extra Kanji with no new name" in {
      val k = ExtraKanji(name, radical, strokes, meaning, reading, kyu, number)
      checkNumberedFields(k)
      assert(k.kanjiType == KanjiType.Extra)
      assert(k.newName.isEmpty)
    }

    "create Extra Kanji with a new name" in {
      val newName = "犬"
      val k = ExtraKanji(name, radical, strokes, meaning, reading, kyu, number,
        Option(newName))
      checkNumberedFields(k)
      assert(k.kanjiType == KanjiType.Extra)
      assert(k.newName.contains(newName))
    }

    "error for Kanji with number <= 0" in {
      Seq(-1, 0).foreach(num =>
        domainException(ExtraKanji(name, radical, strokes, meaning, reading,
            kyu, num), "ExtraKanji: number must be greater than zero")
      )
    }
  }

  "Linked Kanji" - {
    "create Linked Jinmei Kanji" in {
      val link = JouyouKanji(name, radical, strokes, meaning, reading, kyu,
        number, level, frequency, year, Grade.G2)
      val k = LinkedJinmeiKanji(name, radical, strokes, link,
        differentFrequency, differentKyu)
      checkLinkedFields(k, link)
      assert(k.kanjiType == KanjiType.LinkedJinmei)
    }

    "create Linked Old Kanji" in {
      val link = JouyouKanji(name, radical, strokes, meaning, reading, kyu,
        number, level, frequency, year, Grade.G2)
      val k = LinkedOldKanji(name, radical, strokes, link, differentFrequency,
        differentKyu)
      checkLinkedFields(k, link)
      assert(k.kanjiType == KanjiType.LinkedOld)
    }

    "error for Linked Jinmei Kanji with non-official link" in {
      val link =
        ExtraKanji(name, radical, strokes, meaning, reading, kyu, number)
      domainException(LinkedJinmeiKanji(name, radical, strokes, link,
          differentFrequency, differentKyu),
        "LinkedJinmeiKanji: link must be JouyouKanji or JinmeiKanji")
    }

    "error for Linked Old Kanji with non-Jouyou link" in {
      val link =
        ExtraKanji(name, radical, strokes, meaning, reading, kyu, number)
      domainException(LinkedOldKanji(name, radical, strokes, link,
          differentFrequency, differentKyu),
        "LinkedOldKanji: link must be JouyouKanji")
    }
  }

  "Other Kanji" - {
    "create Frequency Kanji" in {
      Seq(HasLinkedReadings, NoLinkedReadings).foreach { linkedReadings =>
        val k = FrequencyKanji(name, radical, strokes, meaning, reading,
          NoOldLinks, Nil, linkedReadings, kyu, frequency)
        checkLoadedFields(k)
        assert(k.kanjiType == KanjiType.Frequency)
        assert(k.oldNames.isEmpty)
        assert(k.newName.isEmpty)
        assert(k.linkedReadings == linkedReadings)
        assert(k.kyu == kyu)
        assert(k.frequency == frequency)
      }
    }

    "error for Frequency Kanji with no frequency" in {
      Seq(-1, 0).foreach(f =>
        domainException(FrequencyKanji(name, radical, strokes, meaning, reading,
            NoOldLinks, Nil, NoLinkedReadings, kyu, f),
          "FrequencyKanji: frequency must be greater than zero")
      )
    }

    "create Kentei Kanji" in {
      Seq(HasLinkedReadings, NoLinkedReadings).foreach { linkedReadings =>
        val k = KenteiKanji(name, radical, strokes, meaning, reading,
          NoOldLinks, Nil, linkedReadings, kyu)
        checkLoadedFields(k)
        assert(k.kanjiType == KanjiType.Kentei)
        assert(k.oldNames.isEmpty)
        assert(k.newName.isEmpty)
        assert(k.linkedReadings == linkedReadings)
        assert(k.kyu == kyu)
        assert(k.frequency == 0)
      }
    }

    "error for Kentei Kanji with no kyu" in {
      domainException(KenteiKanji(name, radical, strokes, meaning, reading,
          NoOldLinks, Nil, NoLinkedReadings, Kyu.None),
        "KenteiKanji: must have a valid kyu")
    }

    "create Ucd Kanji" in {
      Seq(HasLinkedReadings, NoLinkedReadings).foreach { linkedReadings =>
        val k = UcdKanji(name, radical, strokes, meaning, reading, NoOldLinks,
          Nil, linkedReadings)
        checkLoadedFields(k)
        assert(k.kanjiType == KanjiType.Ucd)
        assert(k.oldNames.isEmpty)
        assert(k.newName.isEmpty)
        assert(k.linkedReadings == linkedReadings)
        assert(k.kyu == Kyu.None)
        assert(k.frequency == 0)
      }
    }

    "create Ucd Kanji with old names" in {
      val oldNames = List("辨", "瓣", "辯")
      val k = UcdKanji(name, radical, strokes, meaning, reading, HasOldLinks,
        oldNames, NoLinkedReadings)
      checkLoadedFields(k)
      assert(k.oldNames == oldNames)
      assert(k.newName.isEmpty)
    }

    "create Ucd Kanji with new name" in {
      val newName = "弁"
      val k = UcdKanji(name, radical, strokes, meaning, reading, NoOldLinks,
        List(newName), NoLinkedReadings)
      checkLoadedFields(k)
      assert(k.oldNames.isEmpty)
      assert(k.newName.contains(newName))
    }
  }

  private def checkFields(k: Kanji) = {
    assert(k.name == name)
    assert(k.radical == radical)
    assert(k.strokes == strokes)
  }

  private def checkLinkedFields(k: Kanji, link: Kanji) = {
    assert(k.link == Option(link))
    assert(k.newName.contains(link.name))
    assert(k.meaning == link.meaning)
    assert(k.reading == link.reading)
    assert(k.linkedReadings)
    assert(k.frequency == differentFrequency)
    assert(k.kyu == differentKyu)
    // these should all have default values for linked (ie non-Loaded) Kanji
    assert(k.grade == Grade.None)
    assert(k.level == Level.None)
    assert(k.reason == JinmeiReason.None)
    assert(k.number == 0)
    assert(k.year == 0)
    assert(k.oldNames.isEmpty)
  }

  private def checkLoadedFields(k: Kanji) = {
    assert(k.meaning == meaning)
    assert(k.reading == reading)
    assert(k.link.isEmpty)
    checkFields(k)
  }

  private def checkNumberedFields(k: Kanji) = {
    assert(k.kyu == kyu)
    assert(k.number == number)
    assert(k.oldNames.isEmpty)
    assert(!k.linkedReadings)
    checkLoadedFields(k)
  }

  private def checkOfficialFields(k: Kanji) = {
    assert(k.level == level)
    assert(k.frequency == frequency)
    assert(k.year == year)
    assert(k.newName.isEmpty)
    checkNumberedFields(k)
  }
}

object KanjiTest {
  // sample Kanji fields
  private val name = "海"
  private val radical = "水"
  private val strokes = 9
  private val meaning = "sea"
  private val reading = "カイ、うみ"
  private val kyu = Kyu.K9
  private val differentKyu = Kyu.KJ1
  private val number = 182
  private val level = Level.N4
  private val frequency = 200
  private val differentFrequency = 2489
  private val year = 2023
}
