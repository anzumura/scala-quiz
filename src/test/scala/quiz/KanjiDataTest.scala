package quiz

import java.nio.file.Files

class KanjiDataTest extends FileTest {
  override protected def afterEach(): Unit = {
    EnumListFile.clearEntryData()
    super.afterEach()
  }

  "load[Level] loads all Level files and returns a map of Kanji to Level" in {
    val p = tempDir.resolve("Level")
    Files.createDirectory(p)
    Files.writeString(p.resolve("N5.txt"), "一 二 三\n四 五 六")
    Files.writeString(p.resolve("N4.txt"), "七")
    Files.writeString(p.resolve("N3.txt"), "八")
    Files.writeString(p.resolve("N2.txt"), "九")
    Files.writeString(p.resolve("N1.txt"), "十")
    val data = KanjiData(tempDir)
    Seq("一", "二", "三", "四", "五", "六").foreach(s =>
      assert(data.level(s) == Level.N5)
    )
    assert(data.level("七") == Level.N4)
    assert(data.level("八") == Level.N3)
    assert(data.level("九") == Level.N2)
    assert(data.level("十") == Level.N1)
    assert(data.level("百") == Level.None)
  }
}
