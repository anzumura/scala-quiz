package quiz

import quiz.FileUtils.fileNameStem
import quiz.ListFile.FileType

import java.nio.file.Path

/**
 * holds data loaded from files with Kanji string entries
 *
 * Kanji can be specified either one per line or multiple per line separated by
 * space. Uniqueness is verified when data is loaded and entries are stored in
 * order in a list. There are derived classes for specific data types, i.e.,
 * where all entries are for a 'JLPT Level' or a 'Kentei Kyu'.
 */
class ListFile private (path: Path, fileType: FileType,
    nameIn: Option[String] = None) {
  def index(name: String): Int = 0
  def exists(name: String): Boolean = false
  def name: String = nameIn.getOrElse(fileNameStem(path).capitalize)

  private val entries = Vector.empty[String]
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
