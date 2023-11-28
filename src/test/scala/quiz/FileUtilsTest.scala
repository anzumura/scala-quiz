package quiz

import quiz.FileUtils._
import quiz.FileUtilsTest._

import java.nio.file.{Files, Path}

class FileUtilsTest extends FileTest {
  "fileName returns final component of path" in {
    assert(fileName(testFile) == testFileName)
  }

  "fileName works for path with single component" in {
    assert(fileName(Path.of(testFileName)) == testFileName)
  }

  "fileNameStem removes extensions" in {
    Seq("abc.x.y" -> "abc", "def." -> "def", "abc" -> "abc", "." -> ".",
      ".." -> "..", "..a.b" -> "..a").foreach { case (name, result) =>
      assert(fileNameStem(Path.of(name)) == result)
    }
  }

  "hasExtension returns true if file has an extension" in {
    assert(hasExtension(testFile))
  }

  "hasExtension returns false if file doesn't have an extension" in {
    val fileWithNoExtension = Path.of(testFileBaseName)
    assert(!hasExtension(fileWithNoExtension))
    // '.' in directory name is properly ignored
    assert(!hasExtension(Path.of("abc.def").resolve(fileWithNoExtension)))
  }

  "addExtension to absolute path" in {
    val fileWithNoExtension = tempDir.resolve(testFileBaseName)
    assert(addExtension(fileWithNoExtension, TextFileExtension) == testFile)
  }

  "addExtension to single component" in {
    val fileWithNoExtension = Path.of(testFileBaseName)
    assert(fileName(addExtension(fileWithNoExtension,
        TextFileExtension)) == testFileName)
  }

  "checkExists" - {
    "return given path if it exists" in {
      assert(checkExists(tempDir) == tempDir)
    }

    "return path+extension if path doesn't exist (but path+extension does)" in {
      val fileWithExtension = tempDir.resolve(name + extension)
      Files.createFile(fileWithExtension)
      assert(checkExists(tempDir.resolve(name),
        Option(extension)) == fileWithExtension)
    }

    "error thrown if path doesn't exist" in {
      domainException(checkExists(testFile), s"'$testFile' not found")
    }

    "error has path+extension if neither path nor path+extension exists" in {
      val fileWithExtension = tempDir.resolve(name + extension)
      domainException(checkExists(tempDir.resolve(name), Option(extension)),
        s"'$fileWithExtension' not found")
    }
  }

  "resolve" - {
    "returns absolute path of file in dir" in {
      assert(!Files.exists(testFile))
      Files.createFile(testFile)
      assert(resolve(tempDir, Path.of(testFileName)) == testFile)
    }

    "adds extension if original file doesn't exist" in {
      Files.createFile(testFile)
      assert(resolve(tempDir, Path.of(testFileBaseName),
        Option(TextFileExtension)) == testFile)
    }

    "dir must be a directory" in {
      domainException(resolve(Path.of("bad_dir"), Path.of(testFileName)),
        "'bad_dir' is not a directory")
    }

    "file must not be an absolute path" in {
      domainException(resolve(tempDir, testFile),
        s"'$testFile' is an absolute path")
    }

    "file not found" in {
      domainException(resolve(tempDir, Path.of(testFileName)),
        s"'$testFile' not found")
    }

    "file not found after adding extension" in {
      domainException(resolve(tempDir, Path.of(name), Option(extension)),
        s"'${tempDir.resolve(name + extension)}' not found")
    }

    "file is not a regular file" in {
      domainException(resolve(tempDir.getParent, tempDir.getFileName),
        s"'$tempDir' is not a regular file")
    }
  }

  "textFile calls 'resolve' with '.txt' extension" in {
    Files.createFile(testFile)
    assert(textFile(tempDir, Path.of(testFileBaseName)) == testFile)
  }
}

object FileUtilsTest {
  private val name = "abc"
  private val extension = ".def"
}
