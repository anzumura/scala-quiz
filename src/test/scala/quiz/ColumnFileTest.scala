package quiz

import org.scalatest.funsuite.AnyFunSuite
import ColumnFile.Column
import org.scalatest.{BeforeAndAfterAll, BeforeAndAfterEach}

import java.io.File
import java.nio.file.Files
import scala.jdk.CollectionConverters.SeqHasAsJava

object ColumnFileTest {
  private val testFile = "test.txt"
  private val col1 = Column("col1")
  private val col2 = Column("col2")
  private val col3 = Column("col3")
}

class ColumnFileTest
    extends AnyFunSuite with BeforeAndAfterEach with BeforeAndAfterAll {
  import ColumnFileTest._

  // on Windows this creates a directory under ~/AppData/Local/Temp
  private val tempDir = Files.createTempDirectory("tempDir")

  // delete all files and subdirectories from 'tempDir' after each test
  override protected def afterEach(): Unit = {
    clearDirectory(tempDir.toFile)
  }

  // delete 'tempDir' after all tests
  override protected def afterAll(): Unit = {
    Files.deleteIfExists(tempDir)
  }

  private def clearDirectory(dir: File): Unit = {
    for (file <- dir.listFiles) {
      if (file.isDirectory) clearDirectory(file)
      file.delete
    }
  }

  private def writeTempFile(lines: Seq[String]) = {
    val path = Files.createFile(tempDir.resolve(testFile))
    Files.write(path, lines.asJava)
    path
  }

  private def create(cols: Seq[Column], lines: String*) = {
    ColumnFile(writeTempFile(lines), cols: _*)
  }

  private def create(delimiter: Char, cols: Seq[Column], lines: String*) = {
    ColumnFile(writeTempFile(lines), delimiter, cols: _*)
  }

  test("Column toString is the column name") {
    assert(col1.toString == "col1")
  }

  test("Columns have incrementing numbers") {
    assert(col1.number + 1 == col2.number)
    assert(col2.number + 1 == col3.number)
  }

  test("Column number is unique per name") {
    assert(col1.number == Column(col1.name).number)
    assert(col2.number != Column("col22").number)
  }

  test("Columns with the same name are considered equal") {
    assert(col1 == Column(col1.name)) // same name
    assert(col2 != Column("col22")) // different name
    var x: Any = col3.name
    assert(col3 != x) // classes are different
    x = col3
    assert(col3 == x) // classes are the same
  }
}