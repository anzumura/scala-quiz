package quiz.kanji

import quiz.kanji.KanjiDataTest.*
import quiz.kanji.RadicalData.Radical
import quiz.kanji.UcdData.{LinkType, Sources, Ucd}
import quiz.utils.FileUtils.*
import quiz.utils.UnicodeUtils.Code
import quiz.utils.{EnumListFile, FileTest}

import java.nio.file.Files.isDirectory
import java.nio.file.{Files, Path}

class KanjiDataTest extends FileTest {
  override protected def afterEach(): Unit = {
    EnumListFile.clearEntryData()
    super.afterEach()
  }

  "dataDir returns a valid data directory" in {
    val result = KanjiData.dataDir()
    assert(isDirectory(result))
    assert(fileName(result) == "data")
    assert(getFiles(result).count(_.toString.endsWith(TextFileExtension)) >= 5)
    val dirs = getDirectories(result).map(fileName).toSet
    assert(dirs(Level.enumName))
    assert(dirs(Kyu.enumName))
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
    Seq("一", "二", "三", "四", "五", "六").foreach(s => assert(data.level(s) == Level.N5))
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

  "get frequency starting at 1, or 0 if no frequency" in {
    Files.writeString(tempDir.resolve("frequency.txt"), "一\n二\n三")
    val data = KanjiData(tempDir)
    assert(data.frequency("一") == 1)
    assert(data.frequency("二") == 2)
    assert(data.frequency("三") == 3)
    assert(data.frequency("四") == 0)
  }

  "load jouyou Kanji" in {
    Files.writeString(
      tempDir.resolve("jouyou.txt"),
      """Number	Name	Radical	OldNames	Year	Strokes	Grade	Meaning	Reading
        |1	亜	二	亞		7	S	sub-	ア
        |2	哀	口			9	S	pathetic	アイ、あわ-れ、あわ-れむ
        |3	挨	手		2010	10	S	push open	アイ
        |4	愛	心			13	4	love	アイ
        |""".stripMargin
    )
    val data = new TestKanjiData(tempDir)
    val result = data.getType(KanjiType.Jouyou)
    assert(result.size == 4)
    val k = result(0)
    assert(k.number == 1)
    assert(k.name == "亜")
    assert(k.radical == testRadical)
    assert(k.strokes == 7)
    assert(k.oldNames == List("亞"))
    assert(k.year == 0)
    assert(k.grade == Grade.S)
    assert(k.meaning == "sub-")
    assert(k.reading == "ア")
    // the following values are defaults populated by TestKanjiData class
    assert(k.level == Level.None)
    assert(k.kyu == Kyu.None)
    assert(k.frequency == 0)
    // check year and grade on different Kanji
    assert(result(2).year == 2010)
    assert(result(3).grade == Grade.G4)
  }

  "load jinmei Kanji" in {
    Files.writeString(
      tempDir.resolve("jinmei.txt"),
      """Number	Name	Radical	OldNames	Year	Reason	Reading
        |1	丑	一		1951	Names	チュウ、うし
        |2	丞	一		1951	Names	ジョウ、ショウ、すく-う、たす-ける
        |3	乃	丿		1951	Names	ナイ、ダイ、ノ、アイ、の、すなわ-ち、なんじ
        |4	之	丿		1951	Names	シ、の、これ、おいて、ゆく、この
        |5	乎	丿		2004	Print	コ、オ、か、ああ、かな、や、よ、を
        |6	也	乙		1951	Names	ヤ、エ、なり、か、また
        |7	云	二		2004	Print	ウン、い-う、ここに
        |8	亘	二	亙	1951	Names	コウ、カン、わた-る、もと-める
        |""".stripMargin
    )
    val data = new TestKanjiData(tempDir)
    val result = data.getType(KanjiType.Jinmei)
    assert(result.size == 8)
    val k = result(0)
    assert(k.number == 1)
    assert(k.name == "丑")
    assert(k.radical == testRadical)
    assert(k.strokes == testStrokes)
    assert(k.oldNames == Nil)
    assert(k.year == 1951)
    assert(k.meaning == testMeaning)
    assert(k.reading == "チュウ、うし")
    assert(k.reason == JinmeiReason.Names)
    // the following values are defaults populated by TestKanjiData class
    assert(k.level == Level.None)
    assert(k.kyu == Kyu.None)
    assert(k.frequency == 0)
    // check year, oldNames and reason on different Kanji
    assert(result(4).year == 2004)
    assert(result(4).reason == JinmeiReason.Print)
    assert(result(7).oldNames == List("亙"))
  }
}

object KanjiDataTest {
  // test Radical
  private val testRadical = Radical(1, "一", Nil, "", "")
  private val testRadicalData = new RadicalData(Path.of("")) {
    override def findByName(s: String): Option[Radical] = Option(testRadical)
  }
  // test Ucd
  private val testStrokes = 29
  private val testMeaning = "testMeaning"
  private val testReading = "testReading"
  private val testUcd = Ucd(Code(), testRadical, testStrokes, "", None, Nil,
    Sources("", "", false, false), Nil, LinkType.None, testMeaning, testReading)
  private val testUcdData = new UcdData(Path.of(""), testRadicalData) {
    override def find(s: String): Option[Ucd] = Option(testUcd)
  }

  private class TestKanjiData(path: Path) extends KanjiData(path, testRadicalData, testUcdData) {
    override def level(s: String): Level = Level.None
    override def kyu(s: String): Kyu = Kyu.None
    override def frequency(s: String): Int = 0
  }
}
