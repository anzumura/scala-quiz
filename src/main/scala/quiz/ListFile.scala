package quiz

import quiz.FileUtils.{fileName, fileNameStem}
import quiz.ListFile.{FileType, OnePerLine}

import java.nio.file.Path
import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import scala.util.Using

/**
 * holds data loaded from a file with Kanji string entries
 *
 * Kanji can be specified either one per line or multiple per line separated by
 * space. Uniqueness is verified when data is loaded and entries are stored in
 * order in a list. There are derived classes for specific data types, i.e.,
 * where all entries are for a 'JLPT Level' or a 'Kentei Kyu'.
 */
class ListFile private (path: Path, fileType: FileType,
    nameIn: Option[String] = None)
    extends ThrowsDomainException {

  /**
   * @return name assigned at construction or if no name was given then return
   *         the capitalized file name (without extensions)
   */
  val name: String = nameIn.getOrElse(fileNameStem(path).capitalize)

  /**
   * list of all entries in the file
   */
  val entries: Vector[String] = {
    def err(msg: String, lineNum: Int) =
      error(s"$msg - line: ${lineNum + 1}, file: " + fileName(path))

    val result = ArrayBuffer.empty[String]
    Using(Source.fromFile(path.toFile)) { source =>
      source.getLines().zipWithIndex.foreach { case (line, i) =>
        if (fileType == OnePerLine) {
          if (line.contains(' ')) err("got multiple tokens", i)
          result += line
        } else line.split(' ').foreach(result += _)
      }
    }.failed.foreach(throw _)
    result.toVector
  }

  /**
   * @return number of entries loaded
   */
  def size: Int = entries.size

  /**
   * @param value the value to lookup
   * @return index starting at 0 or None if `name` is not found
   */
  def index(value: String): Option[Int] = entryIndex.get(value)

  /**
   * @param value the value to check
   * @return true if value is contained in this file
   */
  def exists(value: String): Boolean = entryIndex.contains(value)

  private lazy val entryIndex = entries.zipWithIndex.toMap
}

object ListFile extends ThrowsDomainException {
  sealed trait FileType
  case object OnePerLine extends FileType
  case object MultiplePerLine extends FileType

  def apply(path: Path) = new ListFile(path, OnePerLine)
  def apply(path: Path, fileType: FileType) = new ListFile(path, fileType)
  def apply(path: Path, name: String, fileType: FileType = OnePerLine) =
    new ListFile(path, fileType, Option(name))
}
