package quiz.utils

import quiz.kanji.{NoneEnum, NoneEnumObject}
import quiz.test.FileTest
import quiz.utils.ListFile.EntriesPerLine.Multiple

import java.nio.file.Files

enum EnumA extends NoneEnum[EnumA](EnumA) {
  case E1, E2, E3, E4, None
}
object EnumA extends NoneEnumObject[EnumA] {}

enum EnumB extends NoneEnum[EnumB](EnumB) {
  case E1, E2, E3, E4, None
}
object EnumB extends NoneEnumObject[EnumB] {}

class ListFileTest extends FileTest {
  "name is capitalized file name stem by default" in {
    assert(ListFile(writeTestFile()).name == "Test")
  }

  "name can optionally be passed to constructor" in {
    val expected = "abc"
    assert(ListFile(writeTestFile(), expected).name == expected)
  }

  "entries is a list of entries for OnePerLine file" in {
    val f = ListFile(writeTestFile("North\nEast\nSouth\nWest"))
    assert(f.entries == Seq("North", "East", "South", "West"))
  }

  "entries is a list of entries for MultiplePerLine file" in {
    val f = ListFile(writeTestFile("North East South West"), Multiple)
    assert(f.entries == Seq("North", "East", "South", "West"))
  }

  "indices is a map of entry to index" in {
    val f = ListFile(writeTestFile("North\nWest"))
    assert(f.indices == Map("North" -> 0, "West" -> 1))
  }

  "file with multiple entries per line with overridden name" in {
    val expected = "def"
    val f = ListFile(writeTestFile("North East"), expected, Multiple)
    assert(f.name == expected)
  }

  "size returns number of entries loaded" in {
    assert(ListFile(writeTestFile("赤\n青\n黄")).size == 3)
  }

  "empty file has size 0" in { assert(ListFile(writeTestFile()).size == 0) }

  "index returns optional position in entries" in {
    assert(ListFile(writeTestFile("赤\n青\n黄")).index("黄").contains(2))
  }

  "index returns None if not found" in {
    assert(ListFile(writeTestFile("赤\n青\n黄")).index("白").isEmpty)
  }

  "exists returns true if value found in entries" in {
    assert(ListFile(writeTestFile("赤\n青\n黄")).exists("黄"))
  }

  "exists returns false if value not found in entries" in {
    assert(!ListFile(writeTestFile("赤\n青\n黄")).exists("白"))
  }

  "error for multiple entries per line" in {
    fileError(ListFile(writeTestFile("北 東 南 西")).entries, "line has multiple entries", 1)
  }

  "error for duplicate entry" in {
    fileError(ListFile(writeTestFile("北\n東\n北\n西")).entries, "duplicate entry '北'", 3)
  }
}

class KanjiListFileTest extends FileTest {
  "entries contains list of entries for OnePerLine file" in {
    val f = KanjiListFile(writeTestFile("北\n東\n南\n西"))
    assert(f.entries == Seq("北", "東", "南", "西"))
  }

  "entries contains list of entries for MultiplePerLine file" in {
    val f = KanjiListFile(writeTestFile("北 東 南 西"), Multiple)
    assert(f.entries == Seq("北", "東", "南", "西"))
  }

  "file with multiple entries per line with overridden name" in {
    val expected = "def"
    val f = KanjiListFile(writeTestFile("北 東 南 西"), expected, Multiple)
    assert(f.name == expected)
  }

  "error for non-Kanji entry" in {
    fileError(KanjiListFile(writeTestFile("東\nS\n西")).entries, "'S' is not a recognized Kanji", 2)
  }

  "error for entry with more than one Kanji" in {
    fileError(KanjiListFile(writeTestFile("北\n東\n南乾\n西")).entries,
      "'南乾' has more than one Unicode letter", 3)
  }
}

class EnumListFileTest extends FileTest {
  override protected def afterEach(): Unit = {
    EnumListFile.clearEntryData()
    super.afterEach()
  }

  "read entries for a JLPT Level" in {
    Files.writeString(tempDir.resolve("E4.txt"), "一 二 三\n四 五 六")
    val f = EnumListFile(tempDir, EnumA.E4)
    assert(f.size == 6)
    assert(f.value == EnumA.E4)
  }

  "the same entries can exist in files for different enum types" in {
    Files.writeString(tempDir.resolve("E1.txt"), "七 八\n九 十")
    val f1 = EnumListFile(tempDir, EnumA.E1)
    assert(f1.size == 4)
    Files.writeString(tempDir.resolve("E2.txt"), "七 八\n九 十")
    val f2 = EnumListFile(tempDir, EnumB.E2)
    assert(f2.size == 4)
  }

  "entries must be recognized Kanji" in {
    val fileName = "E3.txt"
    Files.writeString(tempDir.resolve(fileName), "北 東 S 西")
    fileError(EnumListFile(tempDir, EnumA.E3).entries, "'S' is not a recognized Kanji", 1, fileName)
  }

  "entries must be unique across all files for the same enum" in {
    Files.writeString(tempDir.resolve("E3.txt"), "七 八\n九 十")
    val f = EnumListFile(tempDir, EnumB.E3)
    assert(f.size == 4)
    val fileName = "E4.txt"
    Files.writeString(tempDir.resolve(fileName), "百 千\n万 八")
    fileError(EnumListFile(tempDir, EnumB.E4).size, s"'八' already in another EnumB", 2, fileName)
  }
}
