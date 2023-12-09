package quiz.utils

import quiz.utils.FileUtils.*
import quiz.utils.FileUtilsTest.*

import java.nio.file.{Files, Path}

class FileUtilsTest extends FileTest {
  "fileName returns final component of path" in {
    assert(fileName(testFile) == testFileName)
  }

  "fileName works for path with single component" in {
    assert(fileName(Path.of(testFileName)) == testFileName)
  }

  "fileNameStem removes extensions" in {
    Seq("abc.x.y" -> "abc", "def." -> "def", "abc" -> "abc", "." -> ".", ".." -> "..",
      "..a.b" -> "..a").foreach { case (name, result) =>
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
    assert(fileName(addExtension(fileWithNoExtension, TextFileExtension)) == testFileName)
  }

  "checkExists" - {
    "return given path if it exists" in {
      assert(checkExists(tempDir) == tempDir)
    }

    "return path+extension if path doesn't exist (but path+extension does)" in {
      val fileWithExtension = tempDir.resolve(name + extension)
      Files.createFile(fileWithExtension)
      assert(checkExists(tempDir.resolve(name), Option(extension)) == fileWithExtension)
    }

    "error thrown if path doesn't exist" in {
      error(checkExists(testFile), s"'$testFile' not found")
    }

    "error has path+extension if neither path nor path+extension exists" in {
      val fileWithExtension = tempDir.resolve(name + extension)
      error(checkExists(tempDir.resolve(name), Option(extension)),
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
      assert(resolve(tempDir, Path.of(testFileBaseName), Option(TextFileExtension)) == testFile)
    }

    "dir must be a directory" in {
      error(resolve(Path.of("bad_dir"), Path.of(testFileName)), "'bad_dir' is not a directory")
    }

    "file must not be an absolute path" in {
      error(resolve(tempDir, testFile), s"'$testFile' is an absolute path")
    }

    "file not found" in {
      error(resolve(tempDir, Path.of(testFileName)), s"'$testFile' not found")
    }

    "file not found after adding extension" in {
      error(resolve(tempDir, Path.of(name), Option(extension)),
        s"'${tempDir.resolve(name + extension)}' not found")
    }

    "file is not a regular file" in {
      error(resolve(tempDir.getParent, tempDir.getFileName), s"'$tempDir' is not a regular file")
    }
  }

  "textFile calls 'resolve' with '.txt' extension" in {
    Files.createFile(testFile)
    assert(textFile(tempDir, testFileBaseName) == testFile)
  }

  "get current working directory" in {
    assert(cwd.toString.nonEmpty)
  }

  "get all files in a directory" in {
    Seq("aa", "bb").foreach(f => Files.createFile(tempDir.resolve(f)))
    Seq("cc", "dd").foreach(f => Files.createDirectory(tempDir.resolve(f)))
    assert(getFiles(tempDir).map(fileName) == Seq("aa", "bb"))
  }

  "get all directories in a directory" in {
    Seq("aa", "bb").foreach(f => Files.createFile(tempDir.resolve(f)))
    Seq("cc", "dd").foreach(f => Files.createDirectory(tempDir.resolve(f)))
    assert(getDirectories(tempDir).map(fileName) == Seq("cc", "dd"))
  }
}

object FileUtilsTest {
  private val name = "abc"
  private val extension = ".def"
}
