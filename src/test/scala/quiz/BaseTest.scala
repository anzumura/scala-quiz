package quiz

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.{BeforeAndAfterAll, BeforeAndAfterEach}

import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.SeqHasAsJava
import scala.jdk.StreamConverters.StreamHasToScala

trait BaseTest extends AnyFreeSpec {
  protected def domainException(f: => Any, msg: String): Unit = {
    val e = intercept[DomainException] { f }
    assert(e.getMessage == msg)
  }

  protected def domainException(f: => Any, t: String => Boolean): Unit = {
    val e = intercept[DomainException] { f }
    assert(t(e.getMessage))
  }
}

trait FileTest extends BaseTest with BeforeAndAfterEach with BeforeAndAfterAll {
  val testFileBaseName: String = "test"
  val testFileName: String = testFileBaseName + ".txt"
  // on Windows tempDir is created in ~/AppData/Local/Temp
  val tempDir: Path = Files.createTempDirectory("tempDir")
  val testFile: Path = tempDir.resolve(testFileName)

  def clearDirectory(d: Path): Unit = {
    Files.walk(d).toScala(LazyList).foreach { f =>
      if (Files.isRegularFile(f)) Files.delete(f)
    }
  }

  def writeTestFile(line: String, lines: String*): Path = {
    val allLines = line +: lines
    Files.write(testFile, allLines.asJava)
  }

  /**
   * write lines to testFile or create empty testFile if lines is empty
   * @param lines lines to write to testFile
   * @return testFile
   */
  def writeTestFile(lines: Seq[String] = Seq.empty[String]): Path = {
    lines.headOption.map(line => writeTestFile(line, lines.tail: _*)).getOrElse(
      Files.createFile(testFile)
    )
  }

  // delete all files from 'tempDir' after each test
  override protected def afterEach(): Unit = {
    clearDirectory(tempDir)
  }

  // delete 'tempDir' after all tests
  override protected def afterAll(): Unit = {
    Files.deleteIfExists(tempDir)
  }
}
