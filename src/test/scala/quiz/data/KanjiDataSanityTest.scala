package quiz.data

import quiz.data.KanjiDataSanityTest.{data, ucdData}
import quiz.kanji.KanjiType.*
import quiz.kanji.{KanjiType, Kyu}
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
}

object KanjiDataSanityTest {
  private val dir = KanjiData.dataDir()
  private val radicalData = RadicalData(dir)
  private val ucdData = UcdData(dir, radicalData)
  class TestKanjiData extends KanjiData(dir, radicalData, ucdData)
  private val data = TestKanjiData()
}
