package quiz

import quiz.KanjiDataTest.TestKanjiData

import java.nio.file.{Files, Path, Paths}

class KanjiDataTest extends FileTest {
  override protected def afterEach(): Unit = {
    EnumListFile.clearEntryData()
  }

  "loadLevels loads all Level files and returns a map of Kanji to Level" in {
    Files.writeString(tempDir.resolve("N5.txt"), "一 二 三\n四 五 六")
    Files.writeString(tempDir.resolve("N4.txt"), "七")
    Files.writeString(tempDir.resolve("N3.txt"), "八")
    Files.writeString(tempDir.resolve("N2.txt"), "九")
    Files.writeString(tempDir.resolve("N1.txt"), "十")
    val data = new TestKanjiData
    val expected = (Seq("一", "二", "三", "四", "五", "六").map(_ -> Level.N5) :+
      ("七" -> Level.N4) :+
      ("八" -> Level.N3) :+
      ("九" -> Level.N2) :+
      ("十" -> Level.N1)).toMap
    assert(data.loadLevels(tempDir) == expected)
  }
}

object KanjiDataTest {
  class TestKanjiData extends KanjiData(Paths.get("")) {
    override def loadLevels(path: Path): Map[String, Level.Value] =
      super.loadLevels(path)
  }
}
