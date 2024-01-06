package quiz.utils

import cats.syntax.all.*
import quiz.utils.Block.isKanji
import quiz.utils.FileUtils.*
import quiz.utils.ListFile.EntriesPerLine

import java.nio.file.Path
import scala.collection.mutable
import scala.io.Source
import scala.util.{Try, Using}

/** holds data loaded from a file with Kanji string entries
 *
 *  Entries can be either one per line or multiple per line separated by space. Uniqueness is
 *  verified when data is loaded, and entries are stored in-order in a list.
 */
class ListFile(path: Path, fileType: EntriesPerLine, nameIn: Option[String])
extends ThrowsDomainException:
  def this(path: Path) = this(path, EntriesPerLine.Single, None)
  def this(path: Path, fileType: EntriesPerLine) = this(path, fileType, None)
  def this(path: Path, name: String, fileType: EntriesPerLine = EntriesPerLine.Single) = this(
    path, fileType, name.some)

  /** @return name assigned at construction or if no name was given then return the capitalized
   *         file name (without extensions)
   */
  val name: String = nameIn.getOrElse(fileNameStem(path).capitalize)

  /** list of all entries in the file */
  lazy val entries: Vector[String] =
    val result = mutable.ArrayBuffer.empty[String]
    Using(Source.fromFile(path.toFile)) { source =>
      val uniqueEntries = mutable.Set.empty[String]
      source.getLines().zip(LazyList.from(1)).foreach { (line, i) =>
        def err(msg: String) = domainError(s"$msg - file: ${fileName(path)}, line: $i")
        def add(entry: String): Unit =
          if !uniqueEntries.add(entry) then err(s"duplicate entry '$entry'")
          Try(if validate(entry) then result += entry).failed.foreach(e => err(e.getMessage))
        if fileType == EntriesPerLine.Single then
          if line.contains(' ') then err("line has multiple entries")
          add(line)
        else line.split(' ').foreach(add)
      }
    }.failed.foreach(throw _)
    result.toVector

  lazy val indices: Map[String, Int] = entries.zipWithIndex.toMap

  /** @return number of entries loaded */
  def size: Int = entries.size

  /** @param value the value to lookup
   *  @return index starting at 0 or None if `name` is not found
   */
  def index(value: String): Option[Int] = indices.get(value)

  /** @param value the value to check
   *  @return true if value is contained in this file
   */
  def exists(value: String): Boolean = indices.contains(value)

  /** derived classes override this method to add validation. A derived class can return true to
   *  allow adding the entry, false to silently skip it or throw an exception which will be caught
   *  and rethrown by the base class
   *
   *  @param entry entry to validate (base class always returns true)
   *  @return true if the entry should be added
   */
  protected def validate(entry: String): Boolean = true

object ListFile:
  enum EntriesPerLine:
    case Single, Multiple

/** derived class of ListFile that ensures each entry is a recognized Kanji */
class KanjiListFile protected (path: Path, fileType: EntriesPerLine, nameIn: Option[String])
extends ListFile(path, fileType, nameIn):
  def this(path: Path) = this(path, EntriesPerLine.Single, None)
  def this(path: Path, fileType: EntriesPerLine) = this(path, fileType, None)
  def this(path: Path, name: String, fileType: EntriesPerLine = EntriesPerLine.Single) = this(
    path, fileType, name.some)

  override protected def validate(entry: String): Boolean =
    isKanji(entry) || error(s"'$entry' is not a recognized Kanji")

/** derived class of [[KanjiListFile]] that ensures each entry is unique across all files for the
 *  same `enum` type, i.e., an entry can't be in more than one JLPT 'Level' file (file name is
 *  `value` plus ".txt", e.g., "N3.txt")
 *  @param dir the directory containing the enum file
 *  @param value enum value, i.e., Level.N3
 */
final class EnumListFile[T <: NoValueEnum[T]](dir: Path, val value: T)
extends KanjiListFile(dir.resolve(value.toString + TextFileExtension), EntriesPerLine.Multiple):

  private val enumEntries = EnumListFile.entries
    .getOrElseUpdate(value.enumName, mutable.Set[String]())

  override protected def validate(entry: String): Boolean =
    super.validate(entry) && enumEntries.add(entry) ||
      error(s"'$entry' already in another ${value.enumName}")

object EnumListFile:
  /** clear all entry data used to ensure uniqueness per enum */
  def clearEntryData(): Unit = entries.clear()

  /** `entries` is used to ensure file entries are unique per enum type. It is a map of 'enum
   *  name' (like "Level" or "Kyu") to a set entries
   */
  private val entries = mutable.Map[String, mutable.Set[String]]()
