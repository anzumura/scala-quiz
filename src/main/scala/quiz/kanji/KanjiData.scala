package quiz.kanji

import quiz.kanji.KanjiData.*
import quiz.utils.*
import quiz.utils.ColumnFile.Column
import quiz.utils.FileUtils.*

import java.nio.file.Files.isDirectory
import java.nio.file.Path
import scala.annotation.tailrec
import scala.collection.mutable

class KanjiData protected (path: Path, radicalData: RadicalData, ucdData: UcdData)
extends ThrowsDomainException {
  /** JLPT level of `s` or "None" if it doesn't have a level */
  def level(s: String): Level = levels.getOrElse(s, Level.None)

  /** Kentei kyu of `s` or "None" if it doesn't have a kyu */
  def kyu(s: String): Kyu = kyus.getOrElse(s, Kyu.None)

  /** frequency of `s` starting at 1 or 0 if it doesn't have a frequency */
  def frequency(s: String): Int = frequencies.getOrElse(s, 0)

  /** get all Kanji for type `t` */
  def getType(t: KanjiType): Vector[Kanji] = types.getOrElseUpdate(t, loadType(t))

  private def loadType(t: KanjiType) = t match {
    case KanjiType.Jouyou => loadJouyou()
    case KanjiType.Jinmei => loadJinmei()
    case KanjiType.Extra => loadExtra()
    case _ => Vector[Kanji]() // load remaining types
  }

  private def loadJouyou() = {
    val gradeCol = Column("Grade")
    val f = ColumnFile(textFile(path, "jouyou"), numberCol, nameCol, radicalCol, oldNamesCol,
      yearCol, strokesCol, gradeCol, meaningCol, readingCol)
    f.processRows(mutable.Buffer[Kanji]()) { result =>
      val name = f.get(nameCol)
      val grade = f.get(gradeCol)
      val oldNames = f.get(oldNamesCol)
      result += JouyouKanji(
        name, getRadical(f.get(radicalCol)), f.getUInt(strokesCol), f.get(meaningCol),
        f.get(readingCol), kyu(name), f.getUInt(numberCol), level(name), frequency(name),
        f.getUIntDefault(yearCol, 0), if (oldNames.isEmpty) Nil else oldNames.split(",").toList,
        Grade.valueOf(if (grade != "S") s"G$grade" else grade))
    }.toVector
  }

  private def loadJinmei() = {
    val reasonCol = Column("Reason")
    val f = ColumnFile(textFile(path, "jinmei"), numberCol, nameCol, radicalCol, oldNamesCol,
      yearCol, reasonCol, readingCol)
    f.processRows(mutable.Buffer[Kanji]()) { result =>
      val name = f.get(nameCol)
      val ucd = getUcd(name)
      val oldNames = f.get(oldNamesCol)
      result += JinmeiKanji(
        name, getRadical(f.get(radicalCol)), ucd.strokes, ucd.meaning, f.get(readingCol), kyu(name),
        f.getUInt(numberCol), level(name), frequency(name), f.getUIntDefault(yearCol, 0),
        if (oldNames.isEmpty) Nil else oldNames.split(",").toList,
        JinmeiReason.valueOf(f.get(reasonCol)))
    }.toVector
  }

  private def loadExtra() = {
    val f = ColumnFile(
      textFile(path, "extra"), numberCol, nameCol, radicalCol, strokesCol, meaningCol, readingCol)
    f.processRows(mutable.Buffer[Kanji]()) { result =>
      val name = f.get(nameCol)
      result += ExtraKanji(name, getRadical(f.get(radicalCol)), f.getUInt(strokesCol),
        f.get(meaningCol), f.get(readingCol), kyu(name), f.getUInt(numberCol))
    }.toVector
  }

  private def load[T <: NoneEnum[T]](e: NoneEnumObject[T]): Map[String, T] = {
    e.defined.map(EnumListFile(path.resolve(e.enumName), _))
      .flatMap(f => f.entries.map(_ -> f.value)).toMap
  }

  private def getUcd(s: String) = ucdData.find(s).getOrElse(error(s"couldn't find Ucd for '$s'"))
  private def getRadical(s: String) = radicalData.findByName(s)
    .getOrElse(error(s"couldn't find Radical '$s'"))

  private lazy val levels = load(Level)
  private lazy val kyus = load(Kyu)
  private lazy val frequencies = KanjiListFile(textFile(path, "frequency")).indices
    .map { case (k, v) => (k, v + 1) }
  private lazy val types = mutable.Map[KanjiType, Vector[Kanji]]()
}

object KanjiData {
  def apply(dir: Path): KanjiData = {
    val radicalData = RadicalData(dir)
    new KanjiData(dir, radicalData, UcdData(dir, radicalData))
  }

  /** returns a path to a "data" directory if a suitable directory can be found in current
   *  working directory or any of it's parent directories
   *  @throws DomainException if a suitable directory isn't found
   */
  @tailrec
  def dataDir(path: Path = cwd): Path = {
    val dir = path.resolve("data")
    if (isDirectory(dir) && fileName(dir) == "data" && hasDataFiles(dir)) dir
    else {
      val parent = path.getParent
      if (parent == path.getRoot) throw DomainException("couldn't find 'data' directory")
      dataDir(path.getParent)
    }
  }

  private def hasDataFiles(dir: Path) = {
    // make sure there are at least 5 ".txt" files
    getFiles(dir).count(_.toString.endsWith(TextFileExtension)) >= 5 && {
      val dirs = getDirectories(dir).map(fileName).toSet
      // make sure dir contains "Level" and "Kyu" subdirectories
      dirs(Level.enumName) && dirs(Kyu.enumName)
    }
  }

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
