package quiz

import collection.mutable

/**
 * class for loading data from a delimiter separated file with a header row
 * containing the column names
 */
class ColumnFile {}

object ColumnFile {
  private val allColumns = mutable.HashMap.empty[String, Int]

  final class Column private (val name: String) {
    val number: Int = allColumns.getOrElseUpdate(name, allColumns.size)

    override def equals(rhs: Any): Boolean = rhs match {
      case c: Column => number == c.number
      case _ => false
    }
    override def hashCode: Int = number.hashCode
    override def toString: String = name
  }
  object Column { def apply(name: String): Column = new Column(name) }
}