package quiz

class ListFileTest extends FileTest {
  "name should be capitalized file name" in {
    assert(ListFile(testFile).name == "Test")
  }
}
