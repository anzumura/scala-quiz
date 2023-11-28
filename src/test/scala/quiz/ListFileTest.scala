package quiz

import quiz.ListFile.MultiplePerLine

class ListFileTest extends FileTest {
  "name is capitalized file name stem by default" in {
    assert(ListFile(testFile).name == "Test")
  }

  "name can optionally be passed to constructor" in {
    val expected = "abc"
    assert(ListFile(testFile, expected).name == expected)
  }

  "entries contains list of entries for OnePerLine file" in {
    val f = ListFile(writeTestFile("北\n東\n南\n西"))
    assert(f.entries == Seq("北", "東", "南", "西"))
  }

  "entries contains list of entries for MultiplePerLine file" in {
    val f = ListFile(writeTestFile("北 東 南 西"), MultiplePerLine)
    assert(f.entries == Seq("北", "東", "南", "西"))
  }

  "size returns number of entries loaded" in {
    assert(ListFile(writeTestFile("赤\n青\n黄")).size == 3)
  }

  "empty file has size 0" in {
    assert(ListFile(writeTestFile(Nil)).size == 0)
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
}
