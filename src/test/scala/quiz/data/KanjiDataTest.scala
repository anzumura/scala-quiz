package quiz.data

import quiz.data.KanjiDataTest.*
import quiz.data.{KanjiData, RadicalData, UcdData}
import quiz.kanji.*
import quiz.kanji.Ucd.{LinkType, Sources}
import quiz.test.FileTest
import quiz.utils.FileUtils.*
import quiz.utils.{Code, EnumListFile}

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
    assert(data.level("百") == Level.NoLevel)
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
    assert(data.kyu("万") == Kyu.NoKyu)
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
        |""".stripMargin)
    val data = new TestKanjiData(tempDir)
    val result = data.getType(KanjiType.Jouyou)
    assert(result.size == 4)
    val k = result("亜")
    assert(data.find("亜").contains(k))
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
    assert(k.level == testLevel)
    assert(k.kyu == testKyu)
    assert(k.frequency == testFrequency)
    // check year and grade on different Kanji
    assert(result("挨").year == 2010)
    assert(result("愛").grade == Grade.G4)
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
        |""".stripMargin)
    val data = new TestKanjiData(tempDir)
    val result = data.getType(KanjiType.Jinmei)
    assert(result.size == 8)
    val k = result("丑")
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
    assert(k.level == testLevel)
    assert(k.kyu == testKyu)
    assert(k.frequency == testFrequency)
    // check year, oldNames and reason on different Kanji
    assert(result("乎").year == 2004)
    assert(result("乎").reason == JinmeiReason.Print)
    assert(result("亘").oldNames == List("亙"))
  }

  "load extra Kanji" in {
    Files.writeString(tempDir.resolve("extra.txt"),
      """Number	Name	Radical	Strokes	Meaning	Reading
        |1	埃	土	10	dust	アイ、ほこり、ちり
        |""".stripMargin)
    val data = new TestKanjiData(tempDir)
    val result = data.getType(KanjiType.Extra)
    assert(result.size == 1)
    val k = result("埃")
    assert(k.number == 1)
    assert(k.name == "埃")
    assert(k.radical == testRadical)
    assert(k.strokes == 10)
    assert(k.meaning == "dust")
    assert(k.reading == "アイ、ほこり、ちり")
    // the following values are defaults populated by TestKanjiData class
    assert(k.kyu == testKyu)
  }

  "load Linked Jinmei Kanji" in {
    Files.writeString(
      tempDir.resolve("jouyou.txt"),
      """Number	Name	Radical	OldNames	Year	Strokes	Grade	Meaning	Reading
        |41	一	一			1	1	one	イチ、イツ、ひと、ひと-つ
        |""".stripMargin)
    createEmptyFiles()
    val data = new TestKanjiData(tempDir)
    val result = data.getType(KanjiType.LinkedJinmei)
    assert(result.size == 1)
    val k = result("二")
    assert(k.name == "二")
    assert(k.radical == testRadical)
    assert(k.strokes == testStrokes)
    assert(k.link.map(_.name).contains("一"))
  }

  "error if Linked Jinmei doesn't have a link field" in {
    createEmptyFiles()
    val noLinkUcd = Ucd(Code(), testRadical, 0, "", None, Nil, Sources("", "", false, false), Nil,
      LinkType.Jinmei, "", "")
    val badUcdData = new UcdData(Path.of(""), testRadicalData) {
      override lazy val data: Map[String, Ucd] = Map("二" -> noLinkUcd)
    }
    val data = new TestKanjiData(tempDir, badUcdData)
    error(data.getType(KanjiType.LinkedJinmei), _.contains("Ucd entry '二' has no link"))
  }

  "error if Linked Jinmei can't find link Kanji" in {
    // need to create both Jouyou and Jinmei files since LinkedJinmeiKanji will check both
    // of these types when trying to find a link
    createEmptyFiles()
    val data = new TestKanjiData(tempDir)
    error(data.getType(KanjiType.LinkedJinmei), _.contains("can't find Kanji for link name '一'"))
  }

  "load Linked Old Kanji" in {
    Files.writeString(
      tempDir.resolve("jouyou.txt"),
      """Number	Name	Radical	OldNames	Year	Strokes	Grade	Meaning	Reading
        |41	一	一			1	1	one	イチ、イツ、ひと、ひと-つ
        |1814	弁	廾	辨,瓣,辯		5	5	valve	ベン
        |""".stripMargin)
    createEmptyFiles()
    val data = new TestKanjiData(tempDir)
    val result = data.getType(KanjiType.LinkedOld)
    assert(result.size == 3)
    val k = result.get("瓣")
    assert(k.nonEmpty)
    assert(k.flatMap(_.newName).contains("弁"))
    assert(k.flatMap(_.link).map(_.name).contains("弁"))
  }

  private def createEmptyFiles(): Unit = {
    val jouyou = tempDir.resolve("jouyou.txt")
    if (!Files.exists(jouyou)) Files.writeString(jouyou,
      """Number	Name	Radical	OldNames	Year	Strokes	Grade	Meaning	Reading
        |""".stripMargin)
    val jinmei = tempDir.resolve("jinmei.txt")
    if (!Files.exists(jinmei)) Files.writeString(jinmei,
      """Number	Name	Radical	OldNames	Year	Reason	Reading
        |""".stripMargin)
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
    Sources("", "", false, false), List(Code("一")), LinkType.Jinmei, testMeaning, testReading)
  private val testUcdData = new UcdData(Path.of(""), testRadicalData) {
    override def find(s: String): Option[Ucd] = Option(testUcd)
    override lazy val data: Map[String, Ucd] = Map("二" -> testUcd)
  }
  // test KanjiData
  private val testLevel = Level.N2
  private val testKyu = Kyu.KJ1
  private val testFrequency = 1234
  private class TestKanjiData(path: Path, ucdData: UcdData = testUcdData)
  extends KanjiData(path, testRadicalData, ucdData) {
    override def level(s: String): Level = testLevel
    override def kyu(s: String): Kyu = testKyu
    override def frequency(s: String): Int = testFrequency
  }
}
