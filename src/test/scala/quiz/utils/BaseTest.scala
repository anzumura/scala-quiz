package quiz.utils

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.{BeforeAndAfterAll, BeforeAndAfterEach}

import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.SeqHasAsJava
import scala.jdk.StreamConverters.StreamHasToScala
import scala.util.Try

trait BaseTest extends AnyFreeSpec {
  /** attempts to return main class name by removing "Test" from this class */
  def mainClassName: String = getClass.getSimpleName.replaceAll("Test", "")

  /** assert that `f` throws a DomainException with given `msg` */
  protected def error(f: => Any, msg: String): Unit = {
    val e = intercept[DomainException] { f }
    assert(e.getMessage == s"$msg")
  }

  /** assert that `f` throws a DomainException and test the message using `t` */
  protected def error(f: => Any, t: String => Boolean): Unit = {
    val e = intercept[DomainException] { f }
    assert(t(e.getMessage))
  }

  /** calls [[error]] with "[mainClassName]" prepended to `msg` */
  protected def domainError(f: => Any, msg: String): Unit = { error(f, s"[$mainClassName] $msg") }
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

  /** write lines to testFile or create empty testFile if lines is empty
   *  @param lines lines to write to testFile
   *  @return testFile
   */
  def writeTestFile(lines: String*): Path = {
    if (lines.isEmpty) Files.createFile(testFile) else Files.write(testFile, lines.asJava)
  }

  // delete all files from 'tempDir' after each test
  override protected def afterEach(): Unit = { clearDirectory(tempDir) }

  // delete 'tempDir' after all tests
  override protected def afterAll(): Unit = { deleteIfExists(tempDir) }

  protected def fileMsg(msg: String, file: Option[String]): String =
    s"$msg - file: ${file.getOrElse(testFileName)}"
  protected def fileMsg(msg: String, line: Int, file: Option[String]): String =
    s"${fileMsg(msg, file)}, line: $line"

  protected def fileError(f: => Any, msg: String): Unit = domainError(f, fileMsg(msg, None))
  protected def fileError(f: => Any, msg: String, line: Int): Unit =
    domainError(f, fileMsg(msg, line, None))
  protected def fileError(f: => Any, msg: String, line: Int, file: String): Unit =
    domainError(f, fileMsg(msg, line, Option(file)))
}
