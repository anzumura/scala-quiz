package quiz.kanji

import quiz.utils.BaseTest

class KanjiTypeTest extends BaseTest {
  "KanjiType has expected values" in {
    import KanjiType.*
    assert(values.toSeq == Seq(Jouyou, Jinmei, LinkedJinmei, LinkedOld,
      Frequency, Extra, Kentei, Ucd))
  }

  "Grade has expected values" in {
    import Grade.*
    assert(defined sameElements Array(G1, G2, G3, G4, G5, G6, S))
    assert(values.filterNot(isDefined) sameElements Array(None))
  }

  "Level has expected values" in {
    import Level.*
    assert(defined sameElements Array(N5, N4, N3, N2, N1))
    assert(values.filterNot(isDefined) sameElements Array(None))
  }

  "Kyu has expected values" in {
    import Kyu.*
    assert(defined sameElements Array(K10, K9, K8, K7, K6, K5, K4, K3, KJ2, K2,
      KJ1, K1))
    assert(values.filterNot(isDefined) sameElements Array(None))
  }

  "JinmeiReason has expected values" in {
    import JinmeiReason.*
    assert(defined sameElements Array(Names, Print, Variant, Moved, Simple,
      Other))
    assert(values.filterNot(isDefined) sameElements Array(None))
  }

  "LinkedReadings has expected values" in {
    import LinkedReadings.*
    assert(values sameElements Array(Yes, No))
  }

  "OldLinks has expected values" in {
    import OldLinks.*
    assert(values sameElements Array(Yes, No))
  }
}
