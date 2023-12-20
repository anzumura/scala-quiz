package quiz.kanji

import quiz.test.BaseTest
import quiz.test.BaseTest.testRadical

class RadicalTest extends BaseTest {
  "toString returns name followed by number in brackets" in {
    assert(testRadical.toString == testRadical.name + s"(${testRadical.number})")
  }

  private val rad1a = Radical(1, "一", Nil, "", "")
  private val rad1b = Radical(1, "二", Nil, "", "")
  private val rad2 = Radical(2, "二", Nil, "", "")

  "equals is only based on Radical number" in {
    // when real data is loaded there won't be two 'different' radicals with the same number
    assert(rad1a == rad1b)
    assert(rad1a != rad2)
    assert(rad1b != rad2)
    val x: Any = rad1a.number
    assert(rad1a != x)
  }

  "hashCode is only based on Radical number" in {
    assert(rad1a.hashCode == rad1b.hashCode)
    assert(rad1a.hashCode != rad2.hashCode)
    assert(rad1b.hashCode != rad2.hashCode)
  }

  "number must be between 1 and 214" in Seq(-1, 0, 215).foreach(number =>
    error(Radical(number, "三", Nil, "", ""), s"Radical number $number not between 1 and 214"))

  "name must be a single Kanji" in Seq("二三", "A", "ア", "あ").foreach(name =>
    error(Radical(1, name, Nil, "", ""), s"Radical name '$name' is not a single Kanji"))

  "each alternative name must be a single Kanji" in Seq("二三", "A", "ア", "あ").foreach(altName =>
    error(Radical(1, "一", List("二", altName, "三"), "", ""),
      s"Radical altName '$altName' is not a single Kanji"))
}
