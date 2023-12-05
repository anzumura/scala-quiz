package quiz

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.{BeforeAndAfterAll, BeforeAndAfterEach}

import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.SeqHasAsJava
import scala.jdk.StreamConverters.StreamHasToScala
import scala.util.Try

trait BaseTest extends AnyFreeSpec {
  /** attempts to return main class name by removing "Test" from this class */
  def mainClassName: String = getClass.getSimpleName.replaceAll("Test", "")

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

  def writeTestFile(line: String, lines: String*): Path = {
    val allLines = line +: lines
    Files.write(testFile, allLines.asJava)
  }

  /** write lines to testFile or create empty testFile if lines is empty
   *  @param lines lines to write to testFile
   *  @return testFile
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
    deleteIfExists(tempDir)
  }

  protected def fileMsg(msg: String, file: Option[String]): String =
    s"[$mainClassName] $msg - file: ${file.getOrElse(testFileName)}"
  protected def fileMsg(msg: String, line: Int, file: Option[String]): String =
    s"${fileMsg(msg, file)}, line: $line"

  protected def fileError(f: => Any, msg: String): Unit =
    domainException(f, fileMsg(msg, None))
  protected def fileError(f: => Any, msg: String, line: Int): Unit =
    domainException(f, fileMsg(msg, line, None))
  protected def fileError(
      f: => Any, msg: String, line: Int, file: String): Unit =
    domainException(f, fileMsg(msg, line, Option(file)))
}
