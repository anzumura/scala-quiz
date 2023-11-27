package quiz

import java.nio.file.{Files, Path}

class ListFileTest extends FileTest {
  "getFile" - {
    "returns absolute path of file in dir" in {
      assert(!Files.exists(testFile))
      Files.createFile(testFile)
      assert(ListFile.getFile(tempDir, Path.of(testFileName)) == testFile)
    }

    "adds '.txt' extension if original file doesn't exist" in {
      Files.createFile(testFile)
      assert(ListFile.getFile(tempDir, Path.of(testFileBaseName)) == testFile)
    }

    "dir must be a directory" in {
      domainException(ListFile.getFile(Path.of("bad_dir"), Path.of(testFileName)),
        "'bad_dir' is not a directory")
    }

    "file must not be an absolute path" in {
      domainException(ListFile.getFile(tempDir, testFile),
        s"'$testFile' is an absolute path")
    }

    "file not found" in {
      domainException(ListFile.getFile(tempDir, Path.of(testFileName)),
        s"'$testFile' not found")
    }

    "file not found after adding extension" in {
      domainException(ListFile.getFile(tempDir, Path.of(testFileBaseName)),
        s"'${tempDir.resolve(testFileName)}' not found")
    }

    "file is not a regular file" in {
      domainException(ListFile.getFile(tempDir.getParent, tempDir.getFileName),
        s"'$tempDir' is not a regular file")
    }
  }
}
