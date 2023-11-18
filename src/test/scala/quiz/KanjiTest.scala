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
}