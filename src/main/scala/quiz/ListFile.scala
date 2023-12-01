package quiz

import quiz.FileUtils.{fileName, fileNameStem}
import quiz.ListFile.{FileType, OnePerLine}
import quiz.UnicodeUtils.isKanji

import java.nio.file.Path
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import scala.util.{Try, Using}

/**
 * holds data loaded from a file with Kanji string entries
 *
 * Entries can be specified either one per line or multiple per line separated
 * by space. Uniqueness is verified when data is loaded and entries are stored
 * in order in a list. There are derived classes for specific data types, i.e.,
 * where all entries are for a 'JLPT Level' or a 'Kentei Kyu'.
 */
class ListFile protected (path: Path, fileType: FileType,
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
  lazy val entries: Vector[String] = {
    val result = ArrayBuffer.empty[String]
    Using(Source.fromFile(path.toFile)) { source =>
      val uniqueEntries = mutable.Set.empty[String]
      source.getLines().zipWithIndex.foreach { case (line, i) =>
        def err(msg: String) =
          error(s"$msg - file: ${fileName(path)}, line: ${i + 1}")
        def add(entry: String): Unit = {
          if (!uniqueEntries.add(entry)) err(s"duplicate entry '$entry'")
          Try(if (validate(entry)) result += entry).failed.foreach(e =>
            err(e.getMessage)
          )
        }
        if (fileType == OnePerLine) {
          if (line.contains(' ')) err("line has multiple entries")
          add(line)
        } else line.split(' ').foreach(add)
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

  /**
   * derived classes can override this method to add validation. A derived class
   * can return true to allow adding the entry, false to silently skip adding it
   * or throw an exception which will be caught and rethrown by the base class
   *
   * @param entry entry to validate (base class always returns true)
   * @return true if the entry should be added
   */
  protected def validate(entry: String): Boolean = true

  private lazy val entryIndex = entries.zipWithIndex.toMap
}

object ListFile {
  sealed trait FileType
  case object OnePerLine extends FileType
  case object MultiplePerLine extends FileType

  def apply(path: Path) = new ListFile(path, OnePerLine)
  def apply(path: Path, fileType: FileType) = new ListFile(path, fileType)
  def apply(path: Path, name: String, fileType: FileType = OnePerLine) =
    new ListFile(path, fileType, Option(name))
}

/**
 * derived class of ListFile that ensures each entry is a recognized Kanji
 */
class KanjiListFile protected (path: Path, fileType: FileType,
    nameIn: Option[String] = None)
    extends ListFile(path, fileType, nameIn) {
  override protected def validate(entry: String): Boolean =
    isKanji(entry) || error(s"'$entry' is not a recognized Kanji")
}

object KanjiListFile {
  def apply(path: Path) = new KanjiListFile(path, OnePerLine)
  def apply(path: Path, fileType: FileType) = new KanjiListFile(path, fileType)
  def apply(path: Path, name: String, fileType: FileType = OnePerLine) =
    new KanjiListFile(path, fileType, Option(name))
}
