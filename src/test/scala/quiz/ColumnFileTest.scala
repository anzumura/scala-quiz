package quiz

import org.scalatest.funsuite.AnyFunSuite

import ColumnFile.Column

object ColumnFileTest {
  private val testFile = "test.txt"
  private val col1 = Column("col1")
  private val col2 = Column("col2")
  private val col3 = Column("col3")
}

class ColumnFileTest extends AnyFunSuite {
  import ColumnFileTest._

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