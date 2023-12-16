package quiz.data

import quiz.data.KanjiDataSanityTest.{data, ucdData}
import quiz.kanji.KanjiType.*
import quiz.kanji.{Grade, KanjiType, Kyu, Level}
import quiz.test.BaseTest

// use real data files for these tests
class KanjiDataSanityTest extends BaseTest {
  "load expected total frequency, level, kyu and ucd data" in {
    assert(data.frequencies.size == 2501)
    assert(data.levels.size == 2222)
    assert(data.kyus.size == 5859)
    assert(ucdData.data.size == 52212)
  }

  "load expected number of each Kanji type" in {
    // the following totals should stay the same even when updating Unicode versions
    Seq(Jouyou -> 2136, Jinmei -> 633, LinkedJinmei -> 230, LinkedOld -> 163, Frequency -> 124)
      .foreach { case (t, expected) => assert(data.getType(t).size == expected) }
    // Extra total could change if Kanji are added or removed from 'extra.txt'
    assert(data.getType(Extra).size == 136)
    // Kentei can change if Extra is changed
    assert(data.getType(Kentei).size == 2822)
    // Ucd can change if Extra is changed, if the parse script is changed or if a new version of
    // Unicode XML data is used (current total is based on Unicode 15.1)
    assert(data.getType(Ucd).size == 45968)
  }

  "load expected total for each Grade" in {
    val result = data.gradeMap
    // every Jouyou Kanji should have a defined grade
    assert(result.values.map(_.size).sum == data.getType(Jouyou).size)
    Grade.values.zip(Seq(80, 160, 200, 200, 185, 181, 1130, 0)).foreach { case (x, n) =>
      assert(result.get(x).map(_.size).getOrElse(0) == n, s" for value: $x")
    }
  }

  "load expected total for each Level" in {
    val result = data.levelMap
    // there should be 2,222 entries with a defined Level value
    assert(result.values.map(_.size).sum == data.levels.size)
    Level.values.zip(Seq(103, 181, 361, 415, 1162, 0)).foreach { case (x, n) =>
      assert(result.get(x).map(_.size).getOrElse(0) == n, s" for value: $x")
    }
  }

  "load expected total for each Kyu" in {
    val totals = Seq(80, 160, 200, 202, 193, 191, 313, 284, 328, 188, 940, 2780, 0)
    // make sure there's an expected 'total' entry for each Kyu enum value
    assert(Kyu.values.length == totals.size)
    // make sure expected totals per Kyu type add up to the total number of Kanji with a Kyu value
    assert(totals.sum == data.kyus.size)

    val result = data.kyuMap
    // there should be 5,859 entries with a defined Kyu value
    assert(result.values.map(_.size).sum == data.kyus.size)
    Kyu.values.zip(totals).foreach { case (x, n) =>
      assert(result.get(x).map(_.size).getOrElse(0) == n, s" for value: $x")
    }
  }

  "check Linked Jinmei link totals" in {
    data.getType(LinkedJinmei).foldLeft((0, 0)) { case ((jouyou, jinmei), (name, k)) =>
      if (k.link.map(_.kanjiType).contains(Jouyou)) (jouyou + 1, jinmei)
      else if (k.link.map(_.kanjiType).contains(Jinmei)) (jouyou, jinmei + 1)
      else fail(s"'$name' had invalid or missing link'")
    } match {
      case (jouyou, jinmei) =>
        assert(jouyou == 212)
        assert(jinmei == 18)
    }
  }

  "check Linked Old Kanji" in {
    val oldKanji = "瓣"
    data.find(oldKanji).map { k =>
      assert(k.kanjiType == LinkedOld)
      assert(k.strokes == 20)
      assert(k.kyu == Kyu.K1)
      assert(k.newName.contains("弁"))
      assert(k.link.map(_.name).contains("弁"))
    }.getOrElse(fail(s"couldn't find '$oldKanji"))
  }

  "check Kanji types with non-zero frequency" in {
    val result = data.frequencyList.foldLeft(Map[KanjiType, Int]()) { case (result, k) =>
      result.updatedWith(k.kanjiType)(_.map(_ + 1).orElse(Option(1)))
    }
    assert(result.values.sum == data.frequencies.size)
    assert(result(Jouyou) == 2037)
    assert(result(Jinmei) == 326)
    assert(result(LinkedJinmei) == 12)
    assert(result(LinkedOld) == 2)
    assert(result(Frequency) == 124)
  }
}

object KanjiDataSanityTest {
  private val dir = KanjiData.dataDir()
  private val radicalData = RadicalData(dir)
  private val ucdData = UcdData(dir, radicalData)
  class TestKanjiData extends KanjiData(dir, radicalData, ucdData)
  private val data = TestKanjiData()
}
