package quiz.test

import cats.syntax.all.*
import org.scalatest.BeforeAndAfterAll
import org.scalatest.freespec.AnyFreeSpec
import quiz.test.FileTest.testFileName

import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.SeqHasAsJava
import scala.jdk.StreamConverters.StreamHasToScala
import scala.util.Try

trait FileTest extends BaseTest with BeforeAndAfterAll {
  // on Windows tempDir is created in ~/AppData/Local/Temp
  val tempDir: Path = Files.createTempDirectory("tempDir")
  val testFile: Path = tempDir.resolve(testFileName)

  def delete(p: Path): Unit = {
    Try(Files.delete(p)).failed.foreach { e =>
      println(s"DELETE FAILED: $e")
      System.exit(1)
    }
  }

  def deleteIfExists(p: Path): Unit = {
    Try(Files.deleteIfExists(p)).failed.foreach { e =>
      println(s"DELETE FAILED: $e")
      System.exit(1)
    }
  }

  def clearDirectory(d: Path): Unit = {
    Files.walk(d).toScala(LazyList).reverse.filter(_ != d).foreach(delete)
  }

  /** write lines to testFile or create empty testFile if lines is empty
   *  @param lines lines to write to testFile
   *  @return testFile
   */
  def writeTestFile(lines: String*): Path = {
    if (lines.isEmpty) Files.createFile(testFile) else Files.write(testFile, lines.asJava)
  }

  // delete all files from 'tempDir' after each test
  override protected def afterEach(): Unit = {
    clearDirectory(tempDir)
    super.afterEach()
  }

  // delete 'tempDir' after all tests
  override protected def afterAll(): Unit = {
    deleteIfExists(tempDir)
    super.afterAll()
  }

  protected def fileMsg(msg: String, file: Option[String]): String =
    s"$msg - file: ${file.getOrElse(testFileName)}"
  protected def fileMsg(msg: String, line: Int, file: Option[String]): String =
    s"${fileMsg(msg, file)}, line: $line"

  protected def fileError(f: => Any, msg: String): Unit = domainError(f, fileMsg(msg, None))
  protected def fileError(f: => Any, msg: String, line: Int): Unit =
    domainError(f, fileMsg(msg, line, None))
  protected def fileError(f: => Any, msg: String, line: Int, file: String): Unit =
    domainError(f, fileMsg(msg, line, file.some))
}

object FileTest {
  val testFileBaseName: String = "test"
  val testFileName: String = testFileBaseName + ".txt"
}
