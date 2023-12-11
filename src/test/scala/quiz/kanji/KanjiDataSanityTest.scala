package quiz.kanji

import quiz.kanji.KanjiDataSanityTest.data
import quiz.utils.BaseTest

// use real data files for these tests
class KanjiDataSanityTest extends BaseTest {
  "load Linked Old Kanji" in {
    val result = data.getType(KanjiType.LinkedOld)
    assert(result.size == 163)
    val k = result.get("瓣")
    assert(k.nonEmpty)
    assert(k.map(_.strokes).contains(20))
    assert(k.map(_.kyu).contains(Kyu.K1))
    assert(k.flatMap(_.newName).contains("弁"))
    assert(k.flatMap(_.link).map(_.name).contains("弁"))
  }
}

object KanjiDataSanityTest {
  private val data = KanjiData(KanjiData.dataDir())
}
