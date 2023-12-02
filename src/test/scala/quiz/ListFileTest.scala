package quiz

import quiz.ListFile.MultiplePerLine

import java.nio.file.Files

class ListFileTest extends FileTest {
  "ListFile" - {
    "name is capitalized file name stem by default" in {
      assert(ListFile(writeTestFile()).name == "Test")
    }

    "name can optionally be passed to constructor" in {
      val expected = "abc"
      assert(ListFile(writeTestFile(), expected).name == expected)
    }

    "entries contains list of entries for OnePerLine file" in {
      val f = ListFile(writeTestFile("North\nEast\nSouth\nWest"))
      assert(f.entries == Seq("North", "East", "South", "West"))
    }

    "entries contains list of entries for MultiplePerLine file" in {
      val f = ListFile(writeTestFile("North East South West"), MultiplePerLine)
      assert(f.entries == Seq("North", "East", "South", "West"))
    }

    "file with multiple entries per line with overridden name" in {
      val expected = "def"
      val f = ListFile(writeTestFile("North East"), expected, MultiplePerLine)
      assert(f.name == expected)
    }

    "size returns number of entries loaded" in {
      assert(ListFile(writeTestFile("赤\n青\n黄")).size == 3)
    }

    "empty file has size 0" in {
      assert(ListFile(writeTestFile()).size == 0)
    }

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
      fileError(ListFile(writeTestFile("北 東 南 西")).entries,
        "line has multiple entries", 1)
    }

    "error for duplicate entry" in {
      fileError(ListFile(writeTestFile("北\n東\n北\n西")).entries,
        "duplicate entry '北'", 3)
    }
  }

  "KanjiListFile" - {
    "entries contains list of entries for OnePerLine file" in {
      val f = KanjiListFile(writeTestFile("北\n東\n南\n西"))
      assert(f.entries == Seq("北", "東", "南", "西"))
    }

    "entries contains list of entries for MultiplePerLine file" in {
      val f = KanjiListFile(writeTestFile("北 東 南 西"), MultiplePerLine)
      assert(f.entries == Seq("北", "東", "南", "西"))
    }

    "file with multiple entries per line with overridden name" in {
      val expected = "def"
      val f = KanjiListFile(writeTestFile("北 東 南 西"), expected, MultiplePerLine)
      assert(f.name == expected)
    }

    "error for non-Kanji entry" in {
      fileError(KanjiListFile(writeTestFile("北\n東\nS\n西")).entries,
        "'S' is not a recognized Kanji", 3)
    }

    "error for entry with more than one Kanji" in {
      fileError(KanjiListFile(writeTestFile("北\n東\n南乾\n西")).entries,
        "'南乾' has more than one Unicode letter", 3)
    }
  }

  "EnumListFile" - {
    "read entries for a JLPT Level" in {
      Files.writeString(tempDir.resolve("N5.txt"), "一 二 三\n四 五 六")
      val f = EnumListFile(tempDir, Level.N5)
      assert(f.size == 6)
      assert(f.enumName == "Level")
      assert(f.value == Level.N5)
    }

    "the same entries can exist in files for different enum types" in {
      Files.writeString(tempDir.resolve("K10.txt"), "七 八\n九 十")
      val kyuFile = EnumListFile(tempDir, Kyu.K10)
      assert(kyuFile.size == 4)
      Files.writeString(tempDir.resolve("G1.txt"), "七 八\n九 十")
      val gradeFile = EnumListFile(tempDir, Grade.G1)
      assert(gradeFile.size == 4)
    }

    "entries must be recognized Kanji" in {
      val fileName = "G2.txt"
      Files.writeString(tempDir.resolve(fileName), "北 東 S 西")
      fileError(EnumListFile(tempDir, Grade.G2).entries,
        "'S' is not a recognized Kanji", 1, fileName)
    }

    "entries must be unique across all files for the same enum" in {
      Files.writeString(tempDir.resolve("N4.txt"), "七 八\n九 十")
      val f = EnumListFile(tempDir, Level.N4)
      assert(f.size == 4)
      val fileName = "N3.txt"
      Files.writeString(tempDir.resolve(fileName), "百 千\n万 八")
      fileError(EnumListFile(tempDir, Level.N3).size,
        s"'八' already in another Level", 2, fileName)
    }
  }
}
