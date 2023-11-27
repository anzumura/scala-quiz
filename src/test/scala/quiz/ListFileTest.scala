package quiz

import java.nio.file.Path

class ListFileTest extends FileTest {
  "getFile error for bad directory" in {
    domainException(ListFile.getFile(Path.of("bad_dir"), Path.of(testFile)),
      "'bad_dir' is not a directory")
  }
}
