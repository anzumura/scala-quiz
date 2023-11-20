package quiz

import collection.mutable
import ColumnFile._

import java.io.IOException
import java.nio.file.Path
import scala.collection.immutable.SortedSet
import scala.io.Source

/**
 * class for loading data from a delimiter separated file with a header row
 * containing the column names
 */
class ColumnFile protected (path: Path, val sep: Char, cols: Seq[Column]) {
  private val fileName = path.getFileName.toString
  private val rowValues = new Array[String](cols.size)
  private val columnToPos = Array.fill(allColumns.size)(ColumnNotFound)
  private val source = Source.fromFile(path.toFile)
  private val lines = source.getLines()

  private var _currentRow = 0

  try {
    if (!lines.hasNext) error("missing header row")
    val colsIn = cols.map(c => (c.name, c)) to mutable.Map
    val colsFound = mutable.Set.empty[String]
    lines.next().split(sep).zipWithIndex.foreach { s =>
      if (!colsFound.add(s._1)) error(s"duplicate header '${s._1}'")
      colsIn.remove(s._1) match {
        case Some(c) => columnToPos(c.number) = s._2
        case _ => error(s"unrecognized header '${s._1}'")
      }
    }
    colsIn.size match {
      case 0 =>
      case 1 => error(s"column '${colsIn.keys.mkString}' not found")
      case s => error(
          s"$s columns not found: '${SortedSet(colsIn.keys).mkString("', '")}'")
    }
  } catch {
    case e: IOException =>
      throw DomainException("failed to read header row: " + e.getMessage)
  }

  def numColumns: Int = rowValues.length
  def currentRow: Int = _currentRow

  // methods to help support testing
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
  val DefaultSeparator: Char = '\t'

  def apply(path: Path, sep: Char, cols: Column*): ColumnFile = {
    if (cols.isEmpty) throw DomainException("must specify at least one column")
    new ColumnFile(path, sep, cols)
  }
  def apply(path: Path, cols: Column*): ColumnFile =
    apply(path, DefaultSeparator, cols: _*)

  private val allColumns = mutable.HashMap.empty[String, Int]
  private val ColumnNotFound, NoMaxValue = -1

  /**
   * represents a column in a <pre>ColumnFile</pre>. Instances are used to get
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