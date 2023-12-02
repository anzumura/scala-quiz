package quiz

class KanjiTypeTest extends BaseTest {
  "KanjiType has expected values" in {
    import KanjiType._
    assert(values.toSeq == Seq(Jouyou, Jinmei, LinkedJinmei, LinkedOld,
      Frequency, Extra, Kentei, Ucd))
  }

  "Grade has expected values" in {
    import Grade._
    assert(values.toSeq == Seq(G1, G2, G3, G4, G5, G6, S, None))
  }

  "Level has expected values" in {
    import Level._
    assert(values.toSeq == Seq(N5, N4, N3, N2, N1, None))
  }

  "Kyu has expected values" in {
    import Kyu._
    assert(values.toSeq == Seq(K10, K9, K8, K7, K6, K5, K4, K3, KJ2, K2, KJ1,
      K1, None))
  }

  "JinmeiReason has expected values" in {
    import JinmeiReason._
    assert(values.toSeq == Seq(Names, Print, Variant, Moved, Simple, Other,
      None))
  }

  "LinkedReadings has expected values" in {
    import LinkedReadings._
    assert(values == Seq(Yes, No))
  }

  "OldLinks has expected values" in {
    import OldLinks._
    assert(values == Seq(Yes, No))
  }
}
