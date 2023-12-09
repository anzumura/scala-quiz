package quiz.kanji

import quiz.kanji.MorohashiId.IndexType.*
import quiz.kanji.MorohashiId.{IndexType, MaxIndex, MaxSupplementalIndex}
import quiz.utils.BaseTest

class MorohashiIdTest extends BaseTest {
  "create" - {
    "plain id" in {
      val m = MorohashiId(12345)
      assert(m.index == 12345)
      assert(m.indexType == Plain)
    }

    "prime id" in {
      val m = MorohashiId(67890, Prime)
      assert(m.index == 67890)
      assert(m.indexType == Prime)
    }

    "double prime id" in {
      val m = MorohashiId(45678, DoublePrime)
      assert(m.index == 45678)
      assert(m.indexType == DoublePrime)
    }

    "supplemental id" in {
      val m = MorohashiId(789, Supplemental)
      assert(m.index == 789)
      assert(m.indexType == Supplemental)
    }
  }

  "create from string" - {
    "plain id" in {
      val m = MorohashiId("12345")
      assert(m.index == 12345)
      assert(m.indexType == Plain)
    }

    "prime id" in {
      val m = MorohashiId("67890P")
      assert(m.index == 67890)
      assert(m.indexType == Prime)
    }

    "double prime id" in {
      val m = MorohashiId("45678PP")
      assert(m.index == 45678)
      assert(m.indexType == DoublePrime)
    }

    "supplemental id" in {
      val m = MorohashiId("H789")
      assert(m.index == 789)
      assert(m.indexType == Supplemental)
    }
  }

  "create error" - {
    "negative index" in { domainError(MorohashiId(-2), "negative index") }

    "index out of range" in {
      val big = MaxIndex + 1
      IndexType.values.filter(_ != Supplemental)
        .foreach(i => domainError(MorohashiId(big, i), s"$i index $big exceeds $MaxIndex"))
    }

    "supplemental index out of range" in {
      val big = MaxSupplementalIndex + 1
      domainError(
        MorohashiId(big, Supplemental), s"$Supplemental index $big exceeds $MaxSupplementalIndex")
    }

    "empty index" in { domainError(MorohashiId(""), "empty index") }

    "invalid format" in {
      Seq("H123P", "123PPP", "PP", "H", "xyz")
        .foreach(s => domainError(MorohashiId(s), s"invalid format '$s'"))
    }
  }

  "to string" - {
    "plain index" in { assert(MorohashiId(12345).toString == "12345") }

    "prime index has trailing 'P'" in { assert(MorohashiId(67890, Prime).toString == "67890P") }

    "double prime index has trailing 'PP'" in {
      assert(MorohashiId(34567, DoublePrime).toString == "34567PP")
    }

    "supplemental index has leading 'H'" in {
      assert(MorohashiId(567, Supplemental).toString == "H567")
    }

    "add leading zeroes if less than 5 digits for non-Supplemental index" in {
      IndexType.values.filter(_ != Supplemental).foreach { i =>
        assert(MorohashiId(1, i).toString.startsWith("00001"))
        assert(MorohashiId(1234, i).toString.startsWith("01234"))
      }
    }

    "add leading zeroes if less than 3 digits for Supplemental index" in {
      assert(MorohashiId(1, Supplemental).toString.startsWith("H001"))
      assert(MorohashiId(12, Supplemental).toString.startsWith("H012"))
    }
  }
}
