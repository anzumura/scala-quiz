package quiz

import quiz.ListFile.MultiplePerLine

class ListFileTest extends FileTest {
  "name is capitalized file name stem by default" in {
    assert(ListFile(writeTestFile()).name == "Test")
  }

  "name can optionally be passed to constructor" in {
    val expected = "abc"
    assert(ListFile(writeTestFile(), expected).name == expected)
  }

  "entries contains list of entries for OnePerLine file" in {
    val f = ListFile(writeTestFile("北\n東\n南\n西"))
    assert(f.entries == Seq("北", "東", "南", "西"))
  }

  "entries contains list of entries for MultiplePerLine file" in {
    val f = ListFile(writeTestFile("北 東 南 西"), MultiplePerLine)
    assert(f.entries == Seq("北", "東", "南", "西"))
  }

  "file with multiple entries per line with overridden name" in {
    val expected = "def"
    val f = ListFile(writeTestFile("北 東 南 西"), expected, MultiplePerLine)
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
    domainException(ListFile(writeTestFile("北 東 南 西")),
      "line has multiple entries - line: 1, file: test.txt")
  }

  "error for duplicate entry" in {
    domainException(ListFile(writeTestFile("北\n東\n北\n西")),
      "duplicate entry '北' - line: 3, file: test.txt")
  }
}
