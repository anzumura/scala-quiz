package quiz

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.{BeforeAndAfterAll, BeforeAndAfterEach}

import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.SeqHasAsJava
import scala.jdk.StreamConverters.StreamHasToScala
import ColumnFile._
import ColumnFileTest._

object ColumnFileTest {
  val cols: Seq[Column] = (1 to 3).map(c => Column("col" + c))
  private val testFile = "test.txt"

  private def clearDirectory(d: Path): Unit = {
    Files.walk(d).toScala(LazyList).foreach { f =>
      if (Files.isRegularFile(f)) Files.delete(f)
    }
  }

  class TestColumnFile(path: Path, sep: Char, cols: Seq[Column])
      extends ColumnFile(path, sep, cols) {
    // make close public so tests can force closing the underlying file
    override def close(): Unit = { super.close() }
  }
}

class ColumnFileTest
    extends AnyFunSuite with BeforeAndAfterEach with BeforeAndAfterAll {
  test("create with one column") {
    assert(1 == create(cols.take(1), "col1").numColumns)
  }

  test("create with multiple columns") {
    assert(2 == create(cols.take(2), "col1\tcol2").numColumns)
  }

  test("create space delimited file") {
    assert(2 == create(' ', cols.take(2), "col1 col2").numColumns)
  }

  test("current row is zero before any data rows are processed") {
    assert(0 == create(cols.take(1), "col1").currentRow)
  }

  test("error for empty columns") {
    val e = intercept[DomainException] { ColumnFile(Path.of("")) }
    assert("must specify at least one column" == e.getMessage)
  }

  test("error for duplicate columns") {
    val c = cols.head
    val e = intercept[DomainException] { ColumnFile(Path.of(""), c, c) }
    assert(s"duplicate column '$c'" == e.getMessage)
  }

  test("error for unable to read file") {
    val e = intercept[DomainException] { ColumnFile(Path.of("x"), cols.head) }
    assert(e.getMessage.startsWith(s"failed to read header row: x"))
  }

  test("error missing header row") {
    val e = intercept[DomainException] { create(cols) }
    assert(errorMsg("missing header row") == e.getMessage)
  }

  // on Windows this creates a directory under ~/AppData/Local/Temp
  private val tempDir = Files.createTempDirectory("tempDir")

  private var testColumnFile = Option.empty[TestColumnFile]

  // delete all files from 'tempDir' after each test
  override protected def afterEach(): Unit = {
    testColumnFile.foreach(_.close())
    clearDirectory(tempDir)
  }

  // delete 'tempDir' after all tests
  override protected def afterAll(): Unit = {
    Files.deleteIfExists(tempDir)
  }

  private def errorMsg(msg: String) = s"$msg - file: $testFile"
  private def errorMsg(msg: String, row: Int): String =
    s"${errorMsg(msg)}, row: $row"
  private def errorMsg(msg: String, row: Int, c: Column, s: String): String =
    s"${errorMsg(msg, row)}, column: '$c', value: '$s'"

  private def writeTempFile(lines: Seq[String]) = {
    val path = Files.createFile(tempDir.resolve(testFile))
    Files.write(path, lines.asJava)
    path
  }

  private def create(sep: Char, cols: Seq[Column], lines: String*) = {
    testColumnFile.foreach(_.close())
    val f = new TestColumnFile(writeTempFile(lines), sep, cols)
    testColumnFile = Option(f)
    f
  }

  private def create(cols: Seq[Column], lines: String*): ColumnFile = {
    create(DefaultSeparator, cols, lines: _*)
  }
}

class ColumnFileColumnTest extends AnyFunSuite {
  test("Column toString is the column name") {
    assert(cols.head.toString == "col1")
  }

  test("Columns have incrementing numbers") {
    cols.iterator.sliding(2).foreach { s =>
      assert(s.head.number + 1 == s.last.number)
    }
  }

  test("Column number is unique per name") {
    cols.foreach { c => assert(c.number == Column(c.name).number) }
  }

  test("Columns with the same name are considered equal") {
    cols.foreach { c =>
      assert(c == Column(c.name))
      assert(c != Column(c.name + "x"))
    }
    var x: Any = cols.head.name
    assert(cols.head != x) // classes are different
    x = cols.head
    assert(cols.head == x) // classes are the same
  }
}