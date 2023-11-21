package quiz

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.{BeforeAndAfterAll, BeforeAndAfterEach}
import quiz.ColumnFile._
import quiz.ColumnFileTest._

import java.io.IOException
import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.SeqHasAsJava
import scala.jdk.StreamConverters.StreamHasToScala

class ColumnFileTest
    extends AnyFreeSpec with BeforeAndAfterEach with BeforeAndAfterAll {

  "create" - {
    "one column" in {
      assert(1 == create(Seq(col1), "col1").numColumns)
    }

    "current row zero before any rows are requested" in {
      assert(0 == create(Seq(col1), "col1").currentRow)
    }

    "multiple columns" in {
      assert(2 == create(cols.take(2), "col1\tcol2").numColumns)
    }

    "space delimited file" in {
      assert(2 == create(' ', cols.take(2), "col1 col2").numColumns)
    }
  }

  "create errors" - {
    "empty columns" in {
      val e = intercept[DomainException] { ColumnFile(Path.of("")) }
      assert("must specify at least one column" == e.getMessage)
    }

    "duplicate columns" in {
      val e = intercept[DomainException] { ColumnFile(Path.of(""), col1, col1) }
      assert(s"duplicate column '$col1'" == e.getMessage)
    }

    "missing file" in {
      val e = intercept[DomainException] { ColumnFile(Path.of("x"), col1) }
      assert(e.getMessage.startsWith(s"failed to read header row: x"))
    }

    "missing header row" in {
      val e = intercept[DomainException] { create(cols) }
      assert(errorMsg("missing header row") == e.getMessage)
    }

    "duplicate header columns" in {
      val e = intercept[DomainException] { create(cols, "col1\tcol1") }
      assert(errorMsg("duplicate header 'col1'") == e.getMessage)
    }

    "unrecognized header column" in {
      val e = intercept[DomainException] { create(cols, "col11") }
      assert(errorMsg("unrecognized header 'col11'") == e.getMessage)
    }

    "one missing column" in {
      val e = intercept[DomainException] { create(cols, "col1\tcol3") }
      assert(errorMsg("column 'col2' not found") == e.getMessage)
    }

    "multiple missing columns" in {
      val e = intercept[DomainException] { create(cols, "col2") }
      assert(errorMsg("2 columns not found: 'col1', 'col3'") == e.getMessage)
    }
  }

  "nextRow" - {
    "called after close" in {
      val f = create(Seq(col1), "col1")
      assert(!f.nextRow())
      val e = intercept[DomainException] { f.nextRow() }
      assert(s"file: '$testFile' has been closed" == e.getMessage)
    }

    "too many columns" in {
      val f = create(Seq(col1), "col1", "A", "B\tC", "D")
      assert(f.nextRow())
      assert(1 == f.currentRow)
      assert("A" == f.get(col1))
      // the second row has two values so an exception is thrown, but current
      // row is incremented so that processing can continue after the bad row
      val e = intercept[DomainException] { f.nextRow() }
      assert(errorMsg("too many columns", 2) == e.getMessage)
      assert(2 == f.currentRow)
      // call nextRow to move to the third row and continue processing
      assert(f.nextRow())
      assert(3 == f.currentRow)
      assert("D" == f.get(col1))
    }

    "not enough columns" in {
      val f = create(cols.take(2), "col1\tcol2", "A", "B\tC")
      val e = intercept[DomainException] { f.nextRow() }
      assert(errorMsg("not enough columns", 1) == e.getMessage)
      // call nextRow to move to the second row and continue processing
      assert(f.nextRow())
      assert(2 == f.currentRow)
      assert("B" == f.get(col1))
      assert("C" == f.get(col2))
    }

    "failed read" in {
      val path = Files.createFile(tempDir.resolve(testFile))
      Files.writeString(path, "col1\nA")
      val f = new TestColumnFile(path, '\t', col1) {
        override def readRow(): String = throw new IOException("bad read")
      }
      val e = intercept[DomainException] { f.nextRow() }
      f.close()
      assert(errorMsg("failed to read next row: bad read") == e.getMessage)
    }

    "failed close" in {
      val path = Files.createFile(tempDir.resolve(testFile))
      Files.writeString(path, "col1")
      val f = new TestColumnFile(path, '\t', col1) {
        override def close(): Unit = {
          super.close()
          throw new IOException("bad close")
        }
      }
      val e = intercept[DomainException] { f.nextRow() }
      assert("failed to close: bad close" == e.getMessage)
    }
  }

  "get" - {
    "string value" in {
      val expected = "Val"
      val f = create(Seq(col1), "col1", expected)
      assert(f.nextRow())
      assert(expected == f.get(col1))
    }

    "values after nextRow returns false" in {
      val f = create(cols.take(2), "col1\tcol2", "A\tB")
      Seq(true, false).foreach { rowRetrieved =>
        assert(rowRetrieved == f.nextRow())
        assert(1 == f.currentRow) // row number doesn't change
        assert("A" == f.get(col1))
        assert("B" == f.get(col2))
      }
    }

    "empty values" in {
      val f = create(cols, "col1\tcol2\tcol3", "\tB\tC", "A\t\tC", "\t\t")
      f.nextRow() // first value is empty
      assert(f.get(col1).isEmpty)
      assert("B" == f.get(col2))
      assert("C" == f.get(col3))
      f.nextRow() // second value is empty
      assert("A" == f.get(col1))
      assert(f.get(col2).isEmpty)
      assert("C" == f.get(col3))
      f.nextRow() // all values are empty
      cols.foreach { c => assert(f.get(c).isEmpty) }
      // make sure all data has been read
      assert(!f.nextRow())
      assert(3 == f.currentRow)
    }

    "before nextRow fails" in {
      val f = create(Seq(col1), "col1", "Val")
      val e = intercept[DomainException] { f.get(col1) }
      assert(errorMsg(
        "'nextRow' must be called before calling 'get'") == e.getMessage)
    }

    "column created after creating ColumnFile is 'unrecognized'" in {
      val f = create(Seq(col1), "col1", "Val")
      assert(f.nextRow())
      val c = Column("Created After")
      val e = intercept[DomainException] { f.get(c) }
      assert(errorMsg("unrecognized column 'Created After'", 1) == e.getMessage)
    }

    "column not included in ColumnFile is 'invalid'" in {
      val c = Column("Created Before")
      val f = create(Seq(col1), "col1", "Val")
      assert(f.nextRow())
      val e = intercept[DomainException] { f.get(c) }
      assert(errorMsg("invalid column 'Created Before'", 1) == e.getMessage)
    }

    "unsigned int value" in {
      val f = create(cols.take(2), "col1\tcol2", "0\t123")
      f.nextRow()
      assert(0 == f.getUInt(col1))
      assert(123 == f.getUInt(col2))
    }

    "invalid unsigned int" in {
      val f = create(cols.take(2), "col1\tcol2", "bad\t-123")
      f.nextRow()
      Seq((col1, "bad"), (col2, "-123")).foreach { case (c, s) =>
        val e = intercept[DomainException] { f.getUInt(c) }
        assert(errorMsg("convert to UInt failed", 1, c, s) == e.getMessage)
      }
    }

    "unsigned int with max value" in {
      val f = create(Seq(col1), "col1", "0", "123")
      f.nextRow()
      assert(0 == f.getUInt(col1, 0))
      f.nextRow()
      assert(123 == f.getUInt(col1, -1))
      assert(123 == f.getUInt(col1, 123))
      assert(123 == f.getUInt(col1, Int.MaxValue))
    }

    "unsigned int exceeding max value" in {
      val f = create(Seq(col1), "col1", "18", "100")
      Seq((0, "18"), (99, "100")).foreach { case (max, s) =>
        f.nextRow()
        val e = intercept[DomainException] { f.getUInt(col1, max) }
        assert(errorMsg(s"exceeded max value $max", f.currentRow, col1,
          s) == e.getMessage)
      }
    }

    "bool values" in {
      val f = create(cols, "col1\tcol2\tcol3", "Y\tT\tx", "N\tF\t")
      f.nextRow()
      assert(f.getBool(col1) && f.getBool(col2))
      val e = intercept[DomainException] { f.getBool(col3) }
      assert(errorMsg("convert to bool failed", 1, col3, "x") == e.getMessage)
      f.nextRow()
      cols.foreach(c => assert(!f.getBool(c)))
    }
  }

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

  private def writeTestFile(lines: Seq[String]) = {
    val path = Files.createFile(tempDir.resolve(testFile))
    Files.write(path, lines.asJava)
    path
  }

  private def create(sep: Char, cols: Seq[Column], lines: String*) = {
    testColumnFile.foreach(_.close())
    val f = new TestColumnFile(writeTestFile(lines), sep, cols: _*)
    testColumnFile = Option(f)
    f
  }

  private def create(cols: Seq[Column], lines: String*): ColumnFile = {
    create(DefaultSeparator, cols, lines: _*)
  }

  private var testColumnFile = Option.empty[TestColumnFile]
}

class ColumnTest extends AnyFreeSpec {
  "toString returns column name" in {
    assert(col1.toString == "col1")
  }

  "number is assigned based on creation order" in {
    cols.iterator.sliding(2).foreach { s =>
      assert(s.head.number + 1 == s.last.number)
    }
  }

  "number is unique per name" in {
    cols.foreach { c => assert(c.number == Column(c.name).number) }
  }

  "same name is considered equal" in {
    cols.foreach { c =>
      assert(c == Column(c.name))
      assert(c != Column(c.name + "x"))
    }
    var x: Any = col1.name
    assert(col1 != x) // classes are different
    x = col1
    assert(col1 == x) // classes are the same
  }
}

object ColumnFileTest {
  val cols: Seq[Column] = (1 to 3).map(c => Column("col" + c))
  val col1 :: col2 :: col3 :: Nil = cols.toList
  private val testFile = "test.txt"
  // on Windows tempDir is created in ~/AppData/Local/Temp
  private val tempDir = Files.createTempDirectory("tempDir")

  private def clearDirectory(d: Path): Unit = {
    Files.walk(d).toScala(LazyList).foreach { f =>
      if (Files.isRegularFile(f)) Files.delete(f)
    }
  }

  private class TestColumnFile(path: Path, sep: Char, cols: Column*)
      extends ColumnFile(path, sep, cols) {
    // make close public so tests can force closing the underlying file
    override def close(): Unit = { super.close() }
  }
}
