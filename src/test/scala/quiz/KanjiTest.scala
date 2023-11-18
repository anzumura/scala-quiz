package quiz

import org.scalatest.funsuite.AnyFunSuite
import Kanji._

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

  private def checkKanjiFields(k: Kanji) = {
    assert(name == k.name)
    assert(radical == k.radical)
    assert(strokes == k.strokes)
  }

  private def checkLinkedKanjiFields(k: Kanji, link: Kanji) = {
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

  test("KanjiType enum has expected values") {
    import Type._
    assert(
      Seq(Jouyou, Jinmei, LinkedJinmei, LinkedOld, Frequency, Extra, Kentei,
        Ucd) sameElements Type.values)
  }

  test("Grade enum has expected values") {
    import Grade._
    assert(Seq(G1, G2, G3, G4, G5, G6, S, None) sameElements Grade.values)
  }

  test("Level enum has expected values") {
    import Level._
    assert(Seq(N5, N4, N3, N2, N1, None) sameElements Level.values)
  }

  test("Kyu enum has expected values") {
    import Kyu._
    assert(Seq(K10, K9, K8, K7, K6, K5, K4, K3, KJ2, K2, KJ1, K1, None)
      sameElements Kyu.values)
  }

  test("JinmeiReason enum has expected values") {
    import JinmeiReason._
    assert(Seq(Names, Print, Variant, Moved, Simple, Other, None) sameElements
      JinmeiReason.values)
  }
}