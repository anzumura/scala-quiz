package quiz

import collection.mutable
import ColumnFile._

import java.nio.file.Path
import java.io.IOException
import scala.io.Source

/**
 * class for loading data from a delimiter separated file with a header row
 * containing the column names
 */
class ColumnFile protected (path: Path, val sep: Char, cols: Seq[Column]) {
  if (cols.isEmpty) throw DomainException("must specify at least one column")

  def numColumns: Int = rowValues.length
  def currentRow: Int = _currentRow

  /**
   * read next row, this method must be called before calling get methods. If
   * there's no more rows then false is returned and the file is closed - thus
   * calling nextRow again after the file is closed raises an exception.
   *
   * @return true if a row was read or false if there is no more data
   * @throws DomainException if reading the next row fails or has incorrect
   *                         number of columns
   */
  def nextRow(): Boolean = {
    if (_closed) throw DomainException(s"file: '$fileName' has been closed")
    val hasNext = lines.hasNext
    if (hasNext) processNextRow()
    else
      try {
        close()
        _closed = true
      } catch {
        case e: IOException =>
          throw DomainException("failed to close: " + e.getMessage)
      }
    hasNext
  }

  /**
   * @param col column contained in this file
   * @return string value for the given column in current row
   * @throws DomainException if nextRow hasn't been called yet or the given
   *                         column isn't part of this file
   */
  def get(col: Column): String = {
    if (_currentRow == 0) error("'nextRow' must be called before calling 'get'")
    if (col.number >= columnToPos.length) error(s"unrecognized column '$col'")
    val pos = columnToPos(col.number)
    if (pos == ColumnNotFound) error(s"invalid column '$col'")
    rowValues(pos)
  }

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

  private def processHeaderRow(source: Source,
      colsIn: mutable.Map[String, Column]) =
    try {
      val lines = source.getLines()
      if (!lines.hasNext) error("missing header row")
      val colsFound = mutable.Set.empty[String]
      lines.next().split(sep).zipWithIndex.foreach { s =>
        if (!colsFound.add(s._1)) error(s"duplicate header '${s._1}'")
        colsIn.remove(s._1) match {
          case Some(c) => columnToPos(c.number) = s._2
          case _ => error(s"unrecognized header '${s._1}'")
        }
      }
      colsIn.size match {
        case 0 => (source, lines)
        case 1 => error(s"column '${colsIn.keys.mkString}' not found")
        case s => error(colsIn.keys.toIndexedSeq.sorted.mkString(
              s"$s columns not found: '", "', '", "'"))
      }
    } catch {
      case e: DomainException =>
        source.close
        throw e
    }

  private def processNextRow(): Unit = {
    try {
      val fields = readRow().split(sep)
      _currentRow += 1
      fields.length match {
        case l if l < numColumns => error("not enough columns")
        case l if l > numColumns => error("too many columns")
        case _ => fields.zipWithIndex.foreach(s => rowValues(s._2) = s._1)
      }
    } catch {
      case e: IOException => error("failed to read next row: " + e.getMessage)
    }
  }

  private val fileName = path.getFileName.toString
  private val rowValues = new Array[String](cols.size)
  private val columnToPos = Array.fill(allColumns.size)(ColumnNotFound)
  private var _currentRow = 0
  private var _closed = false

  private val (source, lines) = {
    val colsIn = mutable.Map.empty[String, Column]
    cols.foreach(c =>
      if (colsIn.put(c.name, c).nonEmpty)
        throw DomainException(s"duplicate column '$c'")
    )
    try {
      processHeaderRow(Source.fromFile(path.toFile), colsIn)
    } catch {
      case e: IOException =>
        throw DomainException("failed to read header row: " + e.getMessage)
    }
  }
}

object ColumnFile {
  val DefaultSeparator: Char = '\t'

  def apply(path: Path, sep: Char, cols: Column*): ColumnFile =
    new ColumnFile(path, sep, cols)
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