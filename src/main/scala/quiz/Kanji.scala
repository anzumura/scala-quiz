package quiz

import Kanji._

sealed abstract class Kanji(private val f: Fields) {
  def name: String = f.name
  def radical: String = f.radical
  def strokes: Int = f.strokes
  // abstract methods
  def kanjiType: KanjiType.Value
  def meaning: String
  def reading: String
  // methods that return default values
  def link: Option[Kanji] = None
  def frequency: Int = 0
  def kyu: Kyu.Value = Kyu.None
  def year: Int = 0
  def hasLinkedReadings: Boolean = false
  def oldNames: List[String] = Nil
  def newName: Option[String] = None
  def grade: Grade.Value = Grade.None
  def level: Level.Value = Level.None
  def number: Int = 0
  def reason: JinmeiReason.Value = JinmeiReason.None

  protected def error(msg: String): Nothing = throw DomainException(
    getClass.getSimpleName + ": " + msg)
}

object Kanji {
  // case classes for holding and passing fields of abstract Kanji classes
  case class Fields(name: String, radical: String, strokes: Int)
  case class LinkedFields(link: Kanji, frequency: Int, kyu: Kyu.Value)
  case class LoadedFields(meaning: String, reading: String)
  case class NumberedFields(kyu: Kyu.Value, number: Int)
  case class OtherFields(oldLinks: Boolean, linkNames: List[String],
    linkedReadings: Boolean)
  case class OfficialFields(level: Level.Value, frequency: Int, year: Int)

  // abstract subclasses of Kanji

  /**
   * base class for the officially recognized variants stored in 'jouyou.txt'
   * and 'jinmei.txt'. Some of these Kanji are in the top 2,501 frequency list
   * and almost all of them are in Kentei KJ1 or K1. However, none of them have
   * a JLPT level. */
  sealed abstract class Linked(f: Fields, private val lf: LinkedFields)
    extends Kanji(f) {
    override def link: Option[Kanji] = Option(lf.link)
    override def frequency: Int = lf.frequency
    override def kyu: Kyu.Value = lf.kyu
    override def meaning: String = lf.link.meaning
    override def reading: String = lf.link.meaning
    override def hasLinkedReadings = true
    override def newName: Option[String] = link.map(_.name)
  }

  /**
   * contains 'meaning' and 'reading' fields loaded from files */
  sealed abstract class Loaded(f: Fields, private val lf: LoadedFields)
    extends Kanji(f) {
    override def meaning: String = lf.meaning
    override def reading: String = lf.reading
  }

  // abstract subclasses of Loaded

  sealed abstract class Numbered(f: Fields, lf: LoadedFields,
    private val nf: NumberedFields) extends Loaded(f, lf) {
    if (nf.number <= 0) error("number must be greater than zero")

    override def kyu: Kyu.Value = nf.kyu
    override def number: Int = nf.number
  }

  sealed abstract class Other(f: Fields, lf: LoadedFields,
    private val of: OtherFields) extends Loaded(f, lf) {

    override def oldNames: List[String] = if (of.oldLinks) of.linkNames else Nil
    override def newName: Option[String] = if (of.oldLinks) None else of
      .linkNames.headOption
    override def hasLinkedReadings: Boolean = of.linkedReadings
  }

  // abstract subclasses of Numbered

  sealed abstract class Official(f: Fields, lf: LoadedFields,
    nf: NumberedFields, private val of: OfficialFields)
    extends Numbered(f, lf, nf) {
    override def level: Level.Value = of.level
    override def frequency: Int = of.frequency
    override def year: Int = of.year
  }

  // abstract subclasses of Other

  sealed abstract class Standard(f: Fields, lf: LoadedFields, of: OtherFields,
    override val kyu: Kyu.Value) extends Other(f, lf, of) {}
}

// concrete subclasses of Kanji.Numbered

final class ExtraKanji(name: String, radical: String, strokes: Int,
  meaning: String, reading: String, kyu: Kyu.Value, number: Int,
  override val newName: Option[String])
  extends Numbered(Fields(name, radical, strokes),
    LoadedFields(meaning, reading), NumberedFields(kyu, number)) {
  override def kanjiType: KanjiType.Value = KanjiType.Extra
}

// concrete subclasses of Kanji.Official

final class JinmeiKanji(name: String, radical: String, strokes: Int,
  meaning: String, reading: String, kyu: Kyu.Value, number: Int,
  level: Level.Value, frequency: Int, year: Int,
  override val reason: JinmeiReason.Value)
  extends Official(Fields(name, radical, strokes),
    LoadedFields(meaning, reading), NumberedFields(kyu, number),
    OfficialFields(level, frequency, year)) {
  // Jinmei Kanji have year values starting at 1951, but for now just ensure
  // it's non-zero to prevent the case of constructing without a year
  if (year == 0) error("must have a valid year")
  if (reason == JinmeiReason.None) error("must have a valid reason")

  override def kanjiType: KanjiType.Value = KanjiType.Jinmei
}

final class JouyouKanji(name: String, radical: String, strokes: Int,
  meaning: String, reading: String, kyu: Kyu.Value, number: Int,
  level: Level.Value, frequency: Int, year: Int,
  override val grade: Grade.Value)
  extends Official(Fields(name, radical, strokes),
    LoadedFields(meaning, reading), NumberedFields(kyu, number),
    OfficialFields(level, frequency, year)) {
  if (grade == Grade.None) error("must have a valid grade")

  override def kanjiType: KanjiType.Value = KanjiType.Jouyou
}

// concrete subclass of Kanji.Other

final class UcdKanji(name: String, radical: String, strokes: Int,
  meaning: String, reading: String, oldLinks: Boolean, linkNames: List[String],
  linkedReadings: Boolean)
  extends Other(Fields(name, radical, strokes), LoadedFields(meaning, reading),
    OtherFields(oldLinks, linkNames, linkedReadings)) {
  override def kanjiType: KanjiType.Value = KanjiType.Ucd
}

// concrete subclasses of Kanji.Standard

final class FrequencyKanji(name: String, radical: String, strokes: Int,
  meaning: String, reading: String, oldLinks: Boolean, linkNames: List[String],
  linkedReadings: Boolean, kyu: Kyu.Value, override val frequency: Int)
  extends Standard(Fields(name, radical, strokes),
    LoadedFields(meaning, reading),
    OtherFields(oldLinks, linkNames, linkedReadings), kyu) {
  override def kanjiType: KanjiType.Value = KanjiType.Frequency
}

final class KenteiKanji(name: String, radical: String, strokes: Int,
  meaning: String, reading: String, oldLinks: Boolean, linkNames: List[String],
  linkedReadings: Boolean, kyu: Kyu.Value, override val frequency: Int)
  extends Standard(Fields(name, radical, strokes),
    LoadedFields(meaning, reading),
    OtherFields(oldLinks, linkNames, linkedReadings), kyu) {
  override def kanjiType: KanjiType.Value = KanjiType.Kentei
}

// concrete subclasses of Kanji.Linked

/**
 * official set of 230 Jinmeiyō Kanji that are old or alternative forms of
 * JouyouKanji or JinmeiKanji. There are 230 of these Kanji:
 * <ul>
 * <li>204 are part of the 365 JouyouKanji 'old names' set
 * <li>8 are different alternate forms of JouyouKanji (薗 駈 嶋 盃 冨 峯 埜 凉)
 * <li> 18 are alternate forms of standard JinmeiKanji
 * </ul> */
final class LinkedJinmeiKanji(name: String, radical: String, strokes: Int,
  link: Kanji, frequency: Int, kyu: Kyu.Value)
  extends Linked(Fields(name, radical, strokes),
    LinkedFields(link, frequency, kyu)) {
  override def kanjiType: KanjiType.Value = KanjiType.LinkedJinmei
}

/**
 * 163 Kanji that link to a JouyouKanji. These are the published Jōyō variants
 * that aren't already included in the 230 Jinmeiyō 'official variants'. */
final class LinkedOldKanji(name: String, radical: String, strokes: Int,
  link: Kanji, frequency: Int, kyu: Kyu.Value)
  extends Linked(Fields(name, radical, strokes),
    LinkedFields(link, frequency, kyu)) {
  override def kanjiType: KanjiType.Value = KanjiType.LinkedOld
}