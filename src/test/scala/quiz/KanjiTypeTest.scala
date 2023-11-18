package quiz

import org.scalatest.funsuite.AnyFunSuite

class KanjiTypeTest extends AnyFunSuite {
  test("KanjiType enum has expected values") {
    import KanjiType._
    assert(
      Seq(Jouyou, Jinmei, LinkedJinmei, LinkedOld, Frequency, Extra, Kentei,
        Ucd) sameElements KanjiType.values)
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