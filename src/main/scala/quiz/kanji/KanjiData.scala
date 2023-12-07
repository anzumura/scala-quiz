package quiz.kanji

import quiz.utils.*
import quiz.utils.ColumnFile.Column
import quiz.utils.FileUtils.*

import java.nio.file.Files.isDirectory
import java.nio.file.Path
import scala.annotation.tailrec
import scala.collection.mutable
import scala.reflect.ClassTag

class KanjiData protected (val path: Path) extends ThrowsDomainException {
  /** JLPT level of `s` or "None" if it doesn't have a level */
  def level(s: String): Level = levels.getOrElse(s, Level.None)

  /** Kentei kyu of `s` or "None" if it doesn't have a kyu */
  def kyu(s: String): Kyu = kyus.getOrElse(s, Kyu.None)

  /** frequency of `s` starting at 1 or 0 if it doesn't have a frequency */
  def frequency(s: String): Int = frequencies.getOrElse(s, 0)

  /** get all Kanji for type `t` */
  def getType(t: KanjiType): Vector[Kanji] =
    types.getOrElse(t, Vector.empty[Kanji])

  private def loadKanji(): mutable.Map[KanjiType, Vector[Kanji]] = {
    val t = mutable.Map[KanjiType, Vector[Kanji]]()
    t += KanjiType.Jouyou -> loadJouyou()
    // load remaining types
    t
  }

  private def loadJouyou() = {
    val gradeCol = Column("Grade")
    val f = ColumnFile(textFile(path, "jouyou"), numberCol, nameCol, radicalCol,
      oldNamesCol, yearCol, strokesCol, gradeCol, meaningCol, readingCol)
    val result = mutable.Buffer[Kanji]()
    while (f.nextRow()) {
      // add validation
      val name = f.get(nameCol)
      val grade = f.get(gradeCol)
      val oldNames = f.get(oldNamesCol)
      result += JouyouKanji(
        name,
        f.get(radicalCol),
        f.getUInt(strokesCol),
        f.get(meaningCol),
        f.get(readingCol),
        kyu(name),
        f.getUInt(numberCol),
        level(name),
        frequency(name),
        f.getUIntDefault(yearCol, 0),
        if (oldNames.isEmpty) Nil else oldNames.split(",").toList,
        Grade.valueOf(if (grade != "S") s"G$grade" else grade)
      )
    }
    result.toVector
  }

  private def load[T <: NoneEnum[T]](e: NoneEnumObject[T]): Map[String, T] = {
    e.defined.map(EnumListFile(path.resolve(e.enumName), _)).flatMap(f =>
      f.entries.map(_ -> f.value)
    ).toMap
  }

  private lazy val levels = load(Level)
  private lazy val kyus = load(Kyu)
  private lazy val frequencies =
    KanjiListFile(textFile(path, "frequency")).indices.map { case (k, v) =>
      (k, v + 1)
    }
  private lazy val types = loadKanji()

  // common columns used for loading Kanji from text files
  private val numberCol = Column("Number")
  private val nameCol = Column("Name")
  private val radicalCol = Column("Radical")
  private val strokesCol = Column("Strokes")
  private val readingCol = Column("Reading")
  private val meaningCol = Column("Meaning")
  private val yearCol = Column("Year")
  private val oldNamesCol = Column("OldNames")
}

object KanjiData {
  def apply(path: Path): KanjiData = new KanjiData(path)

  private def hasDataFiles(dir: Path) = {
    // make sure there are at least 5 ".txt" files
    getFiles(dir).count(_.toString.endsWith(TextFileExtension)) >= 5 && {
      val dirs = getDirectories(dir).map(fileName).toSet
      // make sure dir contains "Level" and "Kyu" subdirectories
      dirs(Level.enumName) && dirs(Kyu.enumName)
    }
  }

  /** returns a path to a "data" directory if a suitable directory can be found
   *  in current working directory or any of it's parent directories
   *  @throws DomainException if a suitable directory isn't found
   */
  @tailrec
  def dataDir(path: Path = cwd): Path = {
    val dir = path.resolve("data")
    if (isDirectory(dir) && fileName(dir) == "data" && hasDataFiles(dir)) dir
    else {
      val parent = path.getParent
      if (parent == path.getRoot)
        throw DomainException("couldn't find 'data' directory")
      dataDir(path.getParent)
    }
  }
}
