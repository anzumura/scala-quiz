package quiz

class KanjiTypeTest extends BaseTest {
  "KanjiType enum has expected values" in {
    import KanjiType._
    assert(Seq(Jouyou, Jinmei, LinkedJinmei, LinkedOld, Frequency, Extra,
      Kentei, Ucd) sameElements values)
  }

  "Grade enum has expected values" in {
    import Grade._
    assert(Seq(G1, G2, G3, G4, G5, G6, S, None) sameElements values)
  }

  "Level enum has expected values" in {
    import Level._
    assert(Seq(N5, N4, N3, N2, N1, None) sameElements values)
  }

  "Kyu enum has expected values" in {
    import Kyu._
    assert(Seq(K10, K9, K8, K7, K6, K5, K4, K3, KJ2, K2, KJ1, K1, None)
      sameElements values)
  }

  "JinmeiReason enum has expected values" in {
    import JinmeiReason._
    assert(Seq(Names, Print, Variant, Moved, Simple, Other, None) sameElements
      values)
  }
}
