package quiz

import java.nio.file.Files

class KanjiDataTest extends FileTest {
  override protected def afterEach(): Unit = {
    EnumListFile.clearEntryData()
    super.afterEach()
  }

  "get JLPT level" in {
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

  "get Kentei kyu" in {
    val sampleKanji = "一二三四五六七八九十百千"
    val p = tempDir.resolve("Kyu")
    Files.createDirectory(p)
    Kyu.defined.zip(sampleKanji).foreach { case (k, c) =>
      Files.writeString(p.resolve(s"$k.txt"), c.toString)
    }
    val data = KanjiData(tempDir)
    assert(data.kyu("一") == Kyu.K10)
    assert(data.kyu("千") == Kyu.K1)
    assert(data.kyu("万") == Kyu.None)
  }

  "get frequency starting at 1 or 0 if no frequency" in {
    Files.writeString(tempDir.resolve("frequency.txt"), "一\n二\n三")
    val data = KanjiData(tempDir)
    assert(data.frequency("一") == 1)
    assert(data.frequency("二") == 2)
    assert(data.frequency("三") == 3)
    assert(data.frequency("四") == 0)
  }
}
