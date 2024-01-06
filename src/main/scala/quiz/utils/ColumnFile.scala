package quiz.utils

import cats.syntax.all.*
import quiz.utils.ColumnFile.*

import java.io.IOException
import java.nio.file.Path
import scala.collection.mutable
import scala.io.Source
import scala.util.{Failure, Success, Try}

/** class for loading data from delimiter separated file with a header row containing column names
 *  @param path file to be processed
 *  @param sep column delimiter
 *  @param allowExtraCols if 'Yes' then file can contain more columns than the list provided in
 *                        `cols`. These 'extra' columns are skipped during processing and can't be
 *                        retrieved via [[get]] methods
 *  @param cols list of columns that are included in the file
 *  @throws DomainException if `cols` is empty or contains a column with a name that's not included
 *                          in the file header row
 *  @throws DomainException if file empty or contains duplicate header names
 *  @throws DomainException if header contains columns that aren't included in `cols` and
 *                          `allowExtraCols` is 'No'
 */
class ColumnFile(path: Path, val sep: Char, allowExtraCols: AllowExtraCols, cols: Column*)
extends ThrowsDomainException:
  if cols.isEmpty then domainError("must specify at least one column")

  def this(path: Path, cols: Column*) = this(path, DefaultSeparator, AllowExtraCols.No, cols*)
  def this(path: Path, allowExtraCols: AllowExtraCols, cols: Column*) = this(
    path, DefaultSeparator, allowExtraCols, cols*)

  /** returns number of columns for this file */
  def numColumns: Int = rowValues.length

  /** returns current row number starting at 1, or 0 if [[nextRow]] hasn't been called yet */
  def currentRow: Int = _currentRow

  /** reads next row and must be called before calling ant [[get]] methods. If there's no more rows
   *  then false is returned and the file is closed - thus calling nextRow again after the file is
   *  closed raises an exception.
   *
   *  @return true if a row was read or false if there is no more data
   *  @throws DomainException if reading the next row fails or has incorrect number of columns
   */
  def nextRow(): Boolean =
    if _closed then domainError(s"file: '$fileName' has been closed")
    val hasNext = lines.hasNext
    if hasNext then processNextRow() else close()
    hasNext

  /** similar to `foldLeft`, but catches exceptions from `op` and properly closes the file before
   *  rethrowing a DomainException that includes the file name and line number. Note, `op` simply
   *  builds up results starting from the initial value `z` (no extra value is passed in per row)
   */
  def processRows[T](z: T)(op: T => T): T =
    var v = z
    Try(while nextRow() do v = op(v)).failed.foreach { e =>
      close()
      // don't add file name, column, etc. if exception already came from this class
      if e.getMessage.startsWith("[" + getClass.getSimpleName) then throw e
      else fileError(e.getMessage)
    }
    v

  def close(): Unit = Try {
    closeFile()
    _closed = true
  }.failed.foreach(e => domainError("failed to close: " + e.getMessage))

  /** @param col column contained in this file
   *  @return string value for the given column in current row
   *  @throws DomainException if [[nextRow]] hasn't been called yet or the given column isn't
   *                          part of this file
   */
  def get(col: Column): String =
    if _currentRow == 0 then fileError("'nextRow' must be called before 'get'")
    if col.number >= columnToPos.length then fileError(s"unknown column '$col'")
    val pos = columnToPos(col.number)
    if pos == ColumnNotFound then fileError(s"invalid column '$col'")
    rowValues(pos)

  /** similar to [[get]], but wraps the return value in a Option (with None for empty string) */
  def getOption(col: Column): Option[String] =
    val x = get(col)
    if x.isEmpty then None else x.some

  /** @param col column contained in this file
   *  @param max max value allowed (only checks if max is non-negative)
   *  @return unsigned int value for the given column in current row
   *  @throws DomainException if [[get]] fails or value can't be converted to an unsigned int that
   *                          is less than or equal to max
   */
  def getUInt(col: Column, max: Int = NoMaxValue): Int = processUInt(get(col), col, max)

  /** similar to [[getUInt]], but if value stored at `col` is empty then return `default`, note,
   *  max check is not performed when `default` is used
   */
  def getUIntDefault(col: Column, default: Int, max: Int = NoMaxValue): Int =
    val s = get(col)
    if s.isEmpty then default else processUInt(s, col, max)

  /** @param col column contained in this file
   *  @return true for "Y" or "T", false for "N", "F" or ""
   *  @throws DomainException if [[get]] fails or value is unrecognized
   */
  def getBool(col: Column): Boolean =
    get(col) match
      case "Y" | "T" => true
      case "N" | "F" | "" => false
      case s => fileError("convert to bool failed", col, s)

  // methods to help support testing
  protected def readRow(): String = lines.next()
  protected def closeFile(): Unit = source.close()

  private def fileError(msg: String) = domainError(errorMsg(msg))
  private def fileError(msg: String, column: Column, s: String) = domainError(
    errorMsg(msg) + s", column: '$column', value: '$s'")

  private def errorMsg(msg: String) =
    val result = s"$msg - file: $fileName"
    if currentRow > 0 then s"$result, line: $currentRow" else result

  private def processHeaderRow(source: Source, colsIn: mutable.Map[String, Column]) =
    Try {
      val lines = source.getLines()
      if !lines.hasNext then fileError("missing header row")
      val colsFound = mutable.Set.empty[String]
      var includedColumnPos = 0
      lines.next().split(sep).zipWithIndex.foreach { (s, pos) =>
        if !colsFound.add(s) then fileError(s"duplicate header '$s'")
        colsIn.remove(s) match
          case Some(c) =>
            columnToPos(c.number) = includedColumnPos
            includedColumnPos += 1
          case _ if allowExtraCols eq AllowExtraCols.Yes => skipColumnPos.add(pos)
          case _ => fileError(s"unrecognized header '$s'")
      }
      colsIn.size match
        case 0 => (source, lines)
        case 1 => fileError(s"column '${colsIn.keys.mkString}' not found")
        case s =>
          fileError(
            colsIn.keys.toIndexedSeq.sorted.mkString(s"$s columns not found: '", "', '", "'"))
    } match
      case Failure(e) =>
        source.close
        throw e
      case Success(x) => x

  private def processNextRow(): Unit =
    Try {
      val vals = readRow().split(splitRegex, -1).zipWithIndex.collect {
        case (s, i) if !skipColumnPos(i) => s
      }
      _currentRow += 1
      vals.length match
        case l if l < numColumns => fileError("not enough columns")
        case l if l > numColumns => fileError("too many columns")
        case _ => vals.zipWithIndex.foreach((s, i) => rowValues(i) = s)
    } match
      case Failure(e: IOException) => fileError(s"failed to read row: ${e.getMessage}")
      case Failure(e) => throw e
      case _ =>

  private def processUInt(s: String, col: Column, max: Int) =
    Try(Integer.parseUnsignedInt(s)) match
      case Failure(_) => fileError("convert to UInt failed", col, s)
      case Success(x) if max >= 0 && max < x => fileError(s"exceeded max value $max", col, s)
      case Success(x) => x

  private val fileName = path.getFileName.toString
  private val rowValues = new Array[String](cols.size)
  private val skipColumnPos = mutable.Set[Int]()
  private val columnToPos = Array.fill(allColumns.size)(ColumnNotFound)
  private val splitRegex = sep.toString // needed for regex split function
  private var _currentRow = 0
  private var _closed = false

  private val (source, lines) =
    val colsIn = mutable.Map.empty[String, Column]
    cols.foreach(c => if colsIn.put(c.name, c).nonEmpty then domainError(s"duplicate column '$c'"))
    Try(processHeaderRow(Source.fromFile(path.toFile), colsIn)) match
      case Success(x) => x
      case Failure(e: IOException) => domainError("failed to read header row: " + e.getMessage)
      case Failure(e) => throw e

object ColumnFile:
  val DefaultSeparator: Char = '\t'

  enum AllowExtraCols:
    case Yes, No

  private val allColumns = mutable.HashMap.empty[String, Int]
  private val ColumnNotFound, NoMaxValue = -1

  /** represents a column in a [[ColumnFile]]. Instances are used to get values from each row and
   *  the same Column instance can be used in multiple ColumnFiles.
   */
  final class Column(val name: String):
    val number: Int = allColumns.getOrElseUpdate(name, allColumns.size)
    override def equals(rhs: Any): Boolean =
      rhs match
        case c: Column => number == c.number
        case _ => false
    override def hashCode: Int = number.hashCode
    override def toString: String = name
