package quiz

import org.scalatest.{BeforeAndAfterAll, BeforeAndAfterEach}
import quiz.ColumnFile._
import quiz.ColumnFileTest._

import java.io.IOException
import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.SeqHasAsJava
import scala.jdk.StreamConverters.StreamHasToScala
import scala.util.Using

class ColumnFileTest
    extends BaseTest with BeforeAndAfterEach with BeforeAndAfterAll {

  "create" - {
    "one column" in {
      assert(create(Seq(col1), "col1").numColumns == 1)
    }

    "current row zero before any rows are requested" in {
      assert(create(Seq(col1), "col1").currentRow == 0)
    }

    "multiple columns" in {
      assert(create(cols.take(2), "col1\tcol2").numColumns == 2)
    }

    "space delimited file" in {
      assert(create(' ', cols.take(2), "col1 col2").numColumns == 2)
    }
  }

  "create errors" - {
    "empty columns" in {
      domainException(ColumnFile(Path.of("")),
        "must specify at least one column")
    }

    "duplicate columns" in {
      domainException(ColumnFile(Path.of(""), col1, col1),
        s"duplicate column '$col1'")
    }

    "missing file" in {
      domainException(ColumnFile(Path.of("x"), col1),
        _.startsWith(s"failed to read header row: x"))
    }

    "missing header row" in {
      fileError(create(cols), "missing header row")
    }

    "duplicate header columns" in {
      fileError(create(cols, "col1\tcol1"), "duplicate header 'col1'")
    }

    "unrecognized header column" in {
      fileError(create(cols, "col11"), "unrecognized header 'col11'")
    }

    "one missing column" in {
      fileError(create(cols, "col1\tcol3"), "column 'col2' not found")
    }

    "multiple missing columns" in {
      fileError(create(cols, "col2"), "2 columns not found: 'col1', 'col3'")
    }
  }

  "nextRow" - {
    "called after close" in {
      val f = create(Seq(col1), "col1")
      assert(!f.nextRow())
      domainException(f.nextRow(), s"file: '$testFile' has been closed")
    }

    "too many columns" in {
      val f = create(Seq(col1), "col1", "A", "B\tC", "D")
      assert(f.nextRow())
      assert(f.currentRow == 1)
      assert(f.get(col1) == "A")
      // the second row has two values so an exception is thrown, but current
      // row is incremented so that processing can continue after the bad row
      fileError(f.nextRow(), "too many columns", 2)
      assert(f.currentRow == 2)
      // call nextRow to move to the third row and continue processing
      assert(f.nextRow())
      assert(f.currentRow == 3)
      assert(f.get(col1) == "D")
    }

    "not enough columns" in {
      val f = create(cols.take(2), "col1\tcol2", "A", "B\tC")
      fileError(f.nextRow(), "not enough columns", 1)
      // call nextRow to move to the second row and continue processing
      assert(f.nextRow())
      assert(f.currentRow == 2)
      assert(f.get(col1) == "B")
      assert(f.get(col2) == "C")
    }

    "failed read" in {
      val path = Files.createFile(tempDir.resolve(testFile))
      Files.writeString(path, "col1\nA")
      Using.resource(new TestColumnFile(path, '\t', col1) {
        override def readRow(): String = throw new IOException("bad read")
      })(f => fileError(f.nextRow(), "failed to read next row: bad read"))
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
      domainException(f.nextRow(), "failed to close: bad close")
    }
  }

  "get" - {
    "string value" in {
      val expected = "Val"
      val f = create(Seq(col1), "col1", expected)
      assert(f.nextRow())
      assert(f.get(col1) == expected)
    }

    "values after nextRow returns false" in {
      val f = create(cols.take(2), "col1\tcol2", "A\tB")
      Seq(true, false).foreach { rowRetrieved =>
        assert(rowRetrieved == f.nextRow())
        assert(f.currentRow == 1) // row number doesn't change
        assert(f.get(col1) == "A")
        assert(f.get(col2) == "B")
      }
    }

    "empty values" in {
      val f = create(cols, "col1\tcol2\tcol3", "\tB\tC", "A\t\tC", "\t\t")
      f.nextRow() // first value is empty
      assert(f.get(col1).isEmpty)
      assert(f.get(col2) == "B")
      assert(f.get(col3) == "C")
      f.nextRow() // second value is empty
      assert(f.get(col1) == "A")
      assert(f.get(col2).isEmpty)
      assert(f.get(col3) == "C")
      f.nextRow() // all values are empty
      cols.foreach { c => assert(f.get(c).isEmpty) }
      // make sure all data has been read
      assert(!f.nextRow())
      assert(f.currentRow == 3)
    }

    "before nextRow fails" in {
      val f = create(Seq(col1), "col1", "Val")
      fileError(f.get(col1), "'nextRow' must be called before calling 'get'")
    }

    "column created after creating ColumnFile is 'unrecognized'" in {
      val f = create(Seq(col1), "col1", "Val")
      assert(f.nextRow())
      val c = Column("Created After")
      fileError(f.get(c), "unrecognized column 'Created After'", 1)
    }

    "column not included in ColumnFile is 'invalid'" in {
      val c = Column("Created Before")
      val f = create(Seq(col1), "col1", "Val")
      assert(f.nextRow())
      fileError(f.get(c), "invalid column 'Created Before'", 1)
    }

    "unsigned int value" in {
      val f = create(cols.take(2), "col1\tcol2", "0\t123")
      f.nextRow()
      assert(f.getUInt(col1) == 0)
      assert(f.getUInt(col2) == 123)
    }

    "invalid unsigned int" in {
      val f = create(cols.take(2), "col1\tcol2", "bad\t-123")
      f.nextRow()
      Seq((col1, "bad"), (col2, "-123")).foreach { case (c, s) =>
        fileError(f.getUInt(c), "convert to UInt failed", 1, c, s)
      }
    }

    "unsigned int with max value" in {
      val f = create(Seq(col1), "col1", "0", "123")
      f.nextRow()
      assert(f.getUInt(col1, 0) == 0)
      f.nextRow()
      assert(f.getUInt(col1, -1) == 123)
      assert(f.getUInt(col1, 123) == 123)
      assert(f.getUInt(col1, Int.MaxValue) == 123)
    }

    "unsigned int exceeding max value" in {
      val f = create(Seq(col1), "col1", "18", "100")
      Seq((0, "18"), (99, "100")).foreach { case (max, s) =>
        f.nextRow()
        fileError(f.getUInt(col1, max), s"exceeded max value $max",
          f.currentRow, col1, s)
      }
    }

    "bool values" in {
      val f = create(cols, "col1\tcol2\tcol3", "Y\tT\tx", "N\tF\t")
      f.nextRow()
      assert(f.getBool(col1) && f.getBool(col2))
      fileError(f.getBool(col3), "convert to bool failed", 1, col3, "x")
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

  private def fileMsg(msg: String): String = s"$msg - file: $testFile"
  private def fileMsg(msg: String, row: Int): String =
    s"${fileMsg(msg)}, row: $row"

  private def fileError(f: => Any, msg: String): Unit =
    domainException(f, fileMsg(msg))
  private def fileError(f: => Any, msg: String, row: Int): Unit =
    domainException(f, fileMsg(msg, row))
  private def fileError(
      f: => Any, msg: String, row: Int, c: Column, s: String): Unit =
    domainException(f, s"${fileMsg(msg, row)}, column: '$c', value: '$s'")

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

class ColumnTest extends BaseTest {
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
      extends ColumnFile(path, sep, cols) with AutoCloseable {
    // make close public so tests can force closing the underlying file
    override def close(): Unit = { super.close() }
  }
}
