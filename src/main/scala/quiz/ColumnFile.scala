package quiz

import collection.mutable
import ColumnFile._

import java.io.{File, IOException}
import scala.collection.immutable.SortedSet
import scala.io.Source

/**
 * class for loading data from a delimiter separated file with a header row
 * containing the column names
 */
class ColumnFile(path: File, val delimiter: Char, columns: Column*) {
  if (columns.isEmpty) throw DomainException("must specify at least one column")
  private val fileName = path.getName
  private val rowValues = new Array[String](columns.size)
  private val columnToPos = Array.fill(allColumns.size)(ColumnNotFound)
  private val source = Source.fromFile(path)
  private val lines = source.getLines()

  private var currentRow = 0

  try {
    if (!lines.hasNext) error("missing header row")
    val cols = columns.map(c => (c.name, c)) to mutable.Map
    val foundCols = mutable.Set.empty[String]
    lines.next().split(delimiter).zipWithIndex.foreach { s =>
      if (!foundCols.add(s._1)) error(s"duplicate header '${s._1}'")
      cols.remove(s._1) match {
        case Some(c) => columnToPos(c.number) = s._2
        case _ => error(s"unrecognized header '${s._1}'")
      }
    }
    cols.size match {
      case 0 =>
      case 1 => error(s"column '${cols.keys.mkString}' not found")
      case s => error(
          s"$s columns not found: '${SortedSet(cols.keys).mkString("', '")}'")
    }
  } catch {
    case e: IOException =>
      throw DomainException("failed to read header row: " + e.getMessage)
  }

  // make these methods protected to help support testing
  protected def readRow(): String = lines.next()
  protected def close(): Unit = source.close()

  private def error(msg: String) = throw DomainException(errorMsg(msg))
  private def error(msg: String, column: Column, s: String) =
    throw DomainException(errorMsg(msg) + s", column: '$column', value: '$s'")

  private def errorMsg(msg: String) = {
    val result = s"$msg - file: $fileName"
    if (currentRow > 0) s"$result, row: $currentRow" else result
  }
}

object ColumnFile {
  private val allColumns = mutable.HashMap.empty[String, Int]
  private val ColumnNotFound, NoMaxValue = -1

  /**
   * represents a column in a {@code ColumnFile}. Instances are used to get
   * values from each row and the same Column instance can be used in multiple
   * ColumnFiles.
   */
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