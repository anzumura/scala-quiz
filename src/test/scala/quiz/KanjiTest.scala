package quiz

import org.scalatest.funsuite.AnyFunSuite

class KanjiTest extends AnyFunSuite {
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

  private def checkFields(k: Kanji) = {
    assert(name == k.name)
    assert(radical == k.radical)
    assert(strokes == k.strokes)
  }

  private def checkLinkedFields(k: Kanji, link: Kanji) = {
    assert(k.link == Option(link))
    assert(k.newName.contains(link.name))
    assert(k.meaning == link.meaning)
    assert(k.reading == link.reading)
    assert(k.hasLinkedReadings)
    assert(differentFrequency == k.frequency)
    assert(differentKyu == k.kyu)
    // these should all have default values for linked (ie non-Loaded) Kanji
    assert(Grade.None == k.grade)
    assert(Level.None == k.level)
    assert(JinmeiReason.None == k.reason)
    assert(0 == k.number)
    assert(0 == k.year)
    assert(k.oldNames.isEmpty)
  }

  private def checkLoadedFields(k: Kanji) = {
    assert(meaning == k.meaning)
    assert(reading == k.reading)
    assert(k.link.isEmpty)
    checkFields(k)
  }

  private def checkNumberedFields(k: Kanji) = {
    assert(kyu == k.kyu)
    assert(number == k.number)
    assert(k.oldNames.isEmpty)
    assert(!k.hasLinkedReadings)
    checkLoadedFields(k)
  }

  private def checkOfficialFields(k: Kanji) = {
    assert(level == k.level)
    assert(frequency == k.frequency)
    assert(year == k.year)
    assert(k.newName.isEmpty)
    checkNumberedFields(k)
  }

  test("create Jouyou Kanji with valid grade") {
    Grade.values.filter(_ != Grade.None).foreach { grade =>
      val k = new JouyouKanji(name, radical, strokes, meaning, reading, kyu,
        number, level, frequency, year, grade)
      checkOfficialFields(k)
      assert(KanjiType.Jouyou == k.kanjiType)
      assert(grade == k.grade)
      assert(JinmeiReason.None == k.reason)
    }
  }

  test("error for Jouyou Kanji with no grade") {
    val e = intercept[DomainException] {
      new JouyouKanji(name, radical, strokes, meaning, reading, kyu, number,
        level, frequency, year, Grade.None)
    }
    assert("JouyouKanji: must have a valid grade" == e.getMessage)
  }

  test("create Jinmei Kanji with valid reason") {
    JinmeiReason.values.filter(_ != JinmeiReason.None).foreach { reason =>
      val k = JinmeiKanji(name, radical, strokes, meaning, reading, kyu, number,
        level, frequency, year, reason)
      checkOfficialFields(k)
      assert(KanjiType.Jinmei == k.kanjiType)
      assert(reason == k.reason)
      assert(Grade.None == k.grade)
    }
  }

  test("error for Jinmei Kanji with no reason") {
    val e = intercept[DomainException] {
      JinmeiKanji(name, radical, strokes, meaning, reading, kyu, number, level,
        frequency, year, JinmeiReason.None)
    }
    assert("JinmeiKanji: must have a valid reason" == e.getMessage)
  }

  test("error for Jinmei Kanji with no year") {
    val e = intercept[DomainException] {
      JinmeiKanji(name, radical, strokes, meaning, reading, kyu, number, level,
        frequency, 0, JinmeiReason.Names)
    }
    assert("JinmeiKanji: must have a valid year" == e.getMessage)
  }

  test("create Extra Kanji with no new name") {
    val k = ExtraKanji(name, radical, strokes, meaning, reading, kyu, number)
    checkNumberedFields(k)
    assert(KanjiType.Extra == k.kanjiType)
    assert(k.newName.isEmpty)
  }

  test("create Extra Kanji with a new name") {
    val newName = "犬"
    val k = ExtraKanji(name, radical, strokes, meaning, reading, kyu, number,
      Option(newName))
    checkNumberedFields(k)
    assert(KanjiType.Extra == k.kanjiType)
    assert(k.newName.contains(newName))
  }

  test("error for Kanji with number <= 0") {
    Seq(-1, 0).foreach { num =>
      val e = intercept[DomainException] {
        ExtraKanji(name, radical, strokes, meaning, reading, kyu, num)
      }
      assert("ExtraKanji: number must be greater than zero" == e.getMessage)
    }
  }

  test("create Linked Jinmei Kanji") {
    val link = new JouyouKanji(name, radical, strokes, meaning, reading, kyu,
      number, level, frequency, year, Grade.G2)
    val k = new LinkedJinmeiKanji(name, radical, strokes, link,
      differentFrequency, differentKyu)
    checkLinkedFields(k, link)
    assert(KanjiType.LinkedJinmei == k.kanjiType)
  }

  test("create Linked Old Kanji") {
    val link = new JouyouKanji(name, radical, strokes, meaning, reading, kyu,
      number, level, frequency, year, Grade.G2)
    val k = new LinkedOldKanji(name, radical, strokes, link, differentFrequency,
      differentKyu)
    checkLinkedFields(k, link)
    assert(KanjiType.LinkedOld == k.kanjiType)
  }

  test("error for Linked Jinmei Kanji with non-official link") {
    val link = ExtraKanji(name, radical, strokes, meaning, reading, kyu, number)
    val e = intercept[DomainException] {
      new LinkedJinmeiKanji(name, radical, strokes, link, differentFrequency,
        differentKyu)
    }
    assert(
      "LinkedJinmeiKanji: link must be JouyouKanji or JinmeiKanji" == e
        .getMessage)
  }

  test("error for Linked Old Kanji with non-Jouyou link") {
    val link = ExtraKanji(name, radical, strokes, meaning, reading, kyu, number)
    val e = intercept[DomainException] {
      new LinkedOldKanji(name, radical, strokes, link, differentFrequency,
        differentKyu)
    }
    assert("LinkedOldKanji: link must be JouyouKanji" == e.getMessage)
  }

  test("create Frequency Kanji") {
    Seq(true, false).foreach { linkedReadings =>
      val k = new FrequencyKanji(name, radical, strokes, meaning, reading,
        false, Nil, linkedReadings, kyu, frequency)
      checkLoadedFields(k)
      assert(KanjiType.Frequency == k.kanjiType)
      assert(k.oldNames.isEmpty)
      assert(k.newName.isEmpty)
      assert(linkedReadings == k.hasLinkedReadings)
      assert(kyu == k.kyu)
      assert(frequency == k.frequency)
    }
  }

  test("error for Frequency Kanji with no frequency") {
    Seq(-1, 0).foreach { f =>
      val e = intercept[DomainException] {
        new FrequencyKanji(name, radical, strokes, meaning, reading, false, Nil,
          false, kyu, f)
      }
      assert(
        "FrequencyKanji: frequency must be greater than zero" == e.getMessage)
    }
  }

  test("create Kentei Kanji") {
    Seq(true, false).foreach { linkedReadings =>
      val k = new KenteiKanji(name, radical, strokes, meaning, reading, false,
        Nil, linkedReadings, kyu)
      checkLoadedFields(k)
      assert(KanjiType.Kentei == k.kanjiType)
      assert(k.oldNames.isEmpty)
      assert(k.newName.isEmpty)
      assert(linkedReadings == k.hasLinkedReadings)
      assert(kyu == k.kyu)
      assert(0 == k.frequency)
    }
  }

  test("error for Kentei Kanji with no kyu") {
    val e = intercept[DomainException] {
      new KenteiKanji(name, radical, strokes, meaning, reading, false, Nil,
        false, Kyu.None)
    }
    assert("KenteiKanji: must have a valid kyu" == e.getMessage)
  }

  test("create Ucd Kanji") {
    Seq(true, false).foreach { linkedReadings =>
      val k = new UcdKanji(name, radical, strokes, meaning, reading, false, Nil,
        linkedReadings)
      checkLoadedFields(k)
      assert(KanjiType.Ucd == k.kanjiType)
      assert(k.oldNames.isEmpty)
      assert(k.newName.isEmpty)
      assert(linkedReadings == k.hasLinkedReadings)
      assert(Kyu.None == k.kyu)
      assert(0 == k.frequency)
    }
  }

  test("create Ucd Kanji with old names") {
    val oldNames = List("辨", "瓣", "辯")
    val k = new UcdKanji(name, radical, strokes, meaning, reading, true,
      oldNames, false)
    checkLoadedFields(k)
    assert(oldNames == k.oldNames)
    assert(k.newName.isEmpty)
  }

  test("create Ucd Kanji with new name") {
    val newName = "弁"
    val k = new UcdKanji(name, radical, strokes, meaning, reading, false,
      List(newName), false)
    checkLoadedFields(k)
    assert(k.oldNames.isEmpty)
    assert(k.newName.contains(newName))
  }
}
