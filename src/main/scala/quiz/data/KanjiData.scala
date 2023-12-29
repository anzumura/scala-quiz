package quiz.data

import cats.syntax.all.*
import quiz.data.KanjiData.*
import quiz.kanji.*
import quiz.kanji.KanjiType.{Jouyou, LinkedJinmei, Ucd}
import quiz.utils.*
import quiz.utils.ColumnFile.Column
import quiz.utils.FileUtils.*

import java.nio.file.Files.isDirectory
import java.nio.file.Path
import scala.annotation.tailrec
import scala.collection.mutable

class KanjiData protected (path: Path, radicalData: RadicalData, ucdData: UcdData)
extends ThrowsDomainException {
  /** returns the Kanji for the string value `s`, i.e., find("空") returns a [[JouyouKanji]]
   *  representing "空" (with strokes, meaning, readings, etc.) or `None` if no Kanji is found.
   *
   *  The method searches for a Kanji from most common to least common [[KanjiType]] and internally
   *  uses a `view` to avoid calling [[getType]] unnecessarily (forcing all types to be loaded)
   */
  def find(s: String): Option[Kanji] = KanjiType.values.view.map(getType(_).get(s))
    .collectFirst { case Some(k) => k }

  /** get all Kanji for type `t` - this method has the side effect of loading Kanji the first time
   *  each type is requested (via the [[loadType]] method)
   */
  def getType(t: KanjiType): Map[String, Kanji] = types.getOrElseUpdate(t, loadType(t))

  // 'level', 'kyu' and 'frequency' methods are used during Kanji creation by the 'load' methods

  /** JLPT level of `s` or "NoLevel" if it doesn't have a level */
  def level(s: String): Level = levels.getOrElse(s, Level.NoLevel)
  /** Kentei kyu of `s` or "NoKyu" if it doesn't have a kyu */
  def kyu(s: String): Kyu = kyus.getOrElse(s, Kyu.NoKyu)
  /** frequency of `s` starting at 1 or 0 if it doesn't have a frequency */
  def frequency(s: String): Int = frequencies.getOrElse(s, 0)

  // lazy val maps used by the above three methods

  /** map of Kanji name to JLPT Level for all Kanji with a JLPT Level */
  lazy val levels: Map[String, Level] = load(Level)
  /** map of Kanji name to Kentei Kyu for all Kanji with a Kentei Kyu */
  lazy val kyus: Map[String, Kyu] = load(Kyu)
  /** map of Kanji name to frequency (starting at 1) for top 2,501 Kanji */
  lazy val frequencies: Map[String, Int] = KanjiListFile(textFile(path, "frequency")).indices
    .map((k, v) => (k, v + 1))

  /** vector of all Kanji with a non-zero frequency in ascending order */
  lazy val frequencyList: Vector[Kanji] = KanjiType.values.takeWhile(_ != KanjiType.Extra)
    .flatMap(getType(_).collect { case (_, k) if k.hasFrequency => k }).sortBy(_.frequency).toVector

  // The following three maps of 'enum type' to 'Vector[Kanji]' are used as quiz types by the main
  // app. For example, a JLPT level 'N2' quiz uses the vector returned from 'levelMap(Level.N2)'
  lazy val gradeMap: Map[Grade, Vector[Kanji]] = enumMap(KanjiType.Jinmei, _.grade)
  lazy val levelMap: Map[Level, Vector[Kanji]] = enumMap(KanjiType.LinkedJinmei, _.level)
  lazy val kyuMap: Map[Kyu, Vector[Kanji]] = enumMap(KanjiType.Ucd, _.kyu)

  private def loadType(t: KanjiType) = t match {
    case KanjiType.Jouyou => loadJouyouKanji()
    case KanjiType.Jinmei => loadJinmeiKanji()
    case KanjiType.LinkedJinmei => loadLinkedJinmeiKanji()
    case KanjiType.LinkedOld => loadLinkedOldKanji()
    case KanjiType.Frequency => loadFrequencyKanji()
    case KanjiType.Extra => loadExtraKanji()
    case KanjiType.Kentei => loadKenteiKanji()
    case KanjiType.Ucd => loadUcdKanji()
  }

  private def loadJouyouKanji() = {
    val gradeCol = Column("Grade")
    val f = ColumnFile(textFile(path, "jouyou"), numberCol, nameCol, radicalCol, oldNamesCol,
      yearCol, strokesCol, gradeCol, meaningCol, readingCol)
    f.processRows(mutable.Buffer[(String, Kanji)]()) { result =>
      val name = f.get(nameCol)
      val grade = f.get(gradeCol)
      val oldNames = f.get(oldNamesCol)
      result +=
        (name -> JouyouKanji(
          name, getRadical(f.get(radicalCol)), f.getUInt(strokesCol), f.get(meaningCol),
          f.get(readingCol), kyu(name), f.getUInt(numberCol), level(name), frequency(name),
          f.getUIntDefault(yearCol, 0), if (oldNames.isEmpty) Nil else oldNames.split(",").toList,
          Grade.valueOf(if (grade != "S") s"G$grade" else grade)))
    }.toMap
  }

  private def loadJinmeiKanji() = {
    val reasonCol = Column("Reason")
    val f = ColumnFile(textFile(path, "jinmei"), numberCol, nameCol, radicalCol, oldNamesCol,
      yearCol, reasonCol, readingCol)
    f.processRows(Map[String, Kanji]()) { result =>
      val name = f.get(nameCol)
      val ucd = getUcd(name)
      val oldNames = f.get(oldNamesCol)
      result +
        (name -> JinmeiKanji(
          name, getRadical(f.get(radicalCol)), ucd.strokes, ucd.meaning, f.get(readingCol),
          kyu(name), f.getUInt(numberCol), level(name), frequency(name),
          f.getUIntDefault(yearCol, 0), if (oldNames.isEmpty) Nil else oldNames.split(",").toList,
          JinmeiReason.valueOf(f.get(reasonCol))))
    }
  }

  private def loadExtraKanji() = {
    val f = ColumnFile(
      textFile(path, "extra"), numberCol, nameCol, radicalCol, strokesCol, meaningCol, readingCol)
    f.processRows(Map[String, Kanji]()) { result =>
      val name = f.get(nameCol)
      result +
        (name -> ExtraKanji(name, getRadical(f.get(radicalCol)), f.getUInt(strokesCol),
          f.get(meaningCol), f.get(readingCol), kyu(name), f.getUInt(numberCol)))
    }
  }

  private def loadLinkedJinmeiKanji() = {
    val jouyou = getType(KanjiType.Jouyou)
    val jinmei = getType(KanjiType.Jinmei)
    ucdData.data.foldLeft(Map[String, Kanji]()) {
      case (result, (name, ucd)) if ucd.linkedJinmei =>
        val linkName = ucd.links.headOption.map(_.toUTF16)
          .getOrElse(domainError(s"Ucd entry '$name' has no link"))
        val linkKanji = jouyou.get(linkName).orElse(jinmei.get(linkName))
          .getOrElse(domainError(s"can't find Kanji for link name '$linkName'"))
        result +
          (name -> LinkedJinmeiKanji(
            name, ucd.radical, ucd.strokes, linkKanji, frequency(name), kyu(name)))
      case (result, _) => result
    }
  }

  private def loadLinkedOldKanji() = {
    val linkedJinmei = getType(KanjiType.LinkedJinmei)
    getType(KanjiType.Jouyou).foldLeft(Map[String, Kanji]()) { case (result, (_, linkKanji)) =>
      linkKanji.oldNames.filterNot(linkedJinmei.contains).foldLeft(result) { (result, name) =>
        val ucd = getUcd(name, domainError)
        result +
          (name ->
            LinkedOldKanji(name, ucd.radical, ucd.strokes, linkKanji, frequency(name), kyu(name)))
      }
    }
  }

  private def loadFrequencyKanji() = {
    // create a FrequencyKanji for any entries in `frequencies` that don't already have an existing
    // Kanji in the first four types (Jouyou, Jinmei, LinkedJinmei and LinkedOld)
    val skip = KanjiType.values.takeWhile(_ != KanjiType.Frequency).map(getType)
    frequencies.filterNot((s, _) => skip.exists(_.contains(s))).foldLeft(Map[String, Kanji]()) {
      case (result, (name, freq)) =>
        val ucd = getUcd(name, domainError)
        result +
          (name -> FrequencyKanji(name, ucd.radical, ucd.strokes, ucd.meaning, ucd.reading,
            ucd.oldLinks, ucd.linkNames, ucd.linkedReadings, kyu(name), freq))
    }
  }

  private def loadKenteiKanji() = {
    // create a KenteiKanji for any entries in `kyus` that don't already have an existing Kanji in
    // the first six types (Jouyou, Jinmei, LinkedJinmei, LinkedOld, Frequency and Extra)
    val skip = KanjiType.values.takeWhile(_ != KanjiType.Kentei).map(getType)
    kyus.filterNot((s, _) => skip.exists(_.contains(s))).foldLeft(Map[String, Kanji]()) {
      case (result, (name, kyu)) =>
        val ucd = getUcd(name, domainError)
        result +
          (name -> KenteiKanji(name, ucd.radical, ucd.strokes, ucd.meaning, ucd.reading,
            ucd.oldLinks, ucd.linkNames, ucd.linkedReadings, kyu))
    }
  }

  private def loadUcdKanji() = {
    // create a UcdKanji for any entries in `ucdData` that don't already have an existing Kanji
    val skip = KanjiType.values.filter(_ != KanjiType.Ucd).map(getType)
    ucdData.data.filterNot((s, _) => skip.exists(_.contains(s))).foldLeft(
      Map[String, Kanji]()) { case (result, (name, ucd)) =>
      result +
        (name -> UcdKanji(name, ucd.radical, ucd.strokes, ucd.meaning, ucd.reading, ucd.oldLinks,
          ucd.linkNames, ucd.linkedReadings))
    }
  }

  private def load[T <: NoValueEnum[T]](e: NoValueEnumObject[T]): Map[String, T] = {
    val result = e.defined.map(EnumListFile(path.resolve(e.enumName), _))
      .flatMap(f => f.entries.map(_ -> f.value)).toMap
    EnumListFile.clearEntryData()
    result
  }

  private def enumMap[T <: NoValueEnum[T]](t: KanjiType, f: Kanji => T) = KanjiType.values
    .takeWhile(_ != t).flatMap(getType).foldLeft(Map[T, Vector[Kanji]]()) { case (result, (_, k)) =>
      f(k).toOption.map(result.updatedWith(_)(_.map(_ :+ k).orElse(Vector(k).some)))
        .getOrElse(result)
    }

  private def getUcd(s: String, f: String => Nothing = error) = ucdData.find(s)
    .getOrElse(f(s"couldn't find Ucd for '$s'"))
  private def getRadical(s: String) = radicalData.findByName(s)
    .getOrElse(error(s"couldn't find Radical '$s'"))

  private lazy val types = mutable.Map[KanjiType, Map[String, Kanji]]()
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
