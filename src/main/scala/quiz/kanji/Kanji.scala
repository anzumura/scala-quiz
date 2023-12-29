package quiz.kanji

import cats.syntax.all.*
import quiz.kanji.Kanji.*
import quiz.utils.DomainException

import scala.language.implicitConversions

sealed abstract class Kanji(val name: String, val radical: Radical, val strokes: Int) {
  override def toString: String = name
  override def equals(obj: Any): Boolean = obj match {
    case k: Kanji => name == k.name
    case _ => false
  }

  /** @param exclude info to exclude from the result
   *  @return string containing info about this instance
   */
  def info(exclude: Info = Info.NoInfo): String = {
    import Info.*
    val result = StringBuilder(s"$name$suffix Rad $radical, Strokes $strokes")
    if (exclude != Frequency && hasFrequency) result ++= s", Frq $frequency"
    if (exclude != Grade && hasGrade) result ++= s", $grade"
    if (exclude != Level && hasLevel) result ++= s", $level"
    if (exclude != Kyu && hasKyu) result ++= s", $kyu"
    result.result()
  }

  // abstract methods

  /** return [[KanjiType]] of this Kanji instance (each concrete leaf class has a unique type) */
  def kanjiType: KanjiType

  /** return single character suffix to help identify how common this Kanji is. The suffixes are:
   *  <ul>
   *  <li><code> . : </code>Jouyou Kanji (the most common type)
   *  <li><code> ' : </code>Jinmei Kanji with a JLPT level (there are 251 N1 Kanji that are Jinmei)
   *  <li><code> &#94; : </code>Jinmei Kanji without a JLPT level
   *  <li><code> ~ : </code>Linked Jinmei Kanji
   *  <li><code> % : </code>Linked Old Kanji
   *  <li><code> " : </code>Frequency Kanji
   *  <li><code> + : </code>Extra Kanji
   *  <li><code> @ : </code>Kentei Kanji (up to KJ1)
   *  <li><code> # : </code>Kentei Kanji (K1)
   *  <li><code> * : </code>Ucd Kanji
   *  </ul>
   */
  def suffix: Char

  /** return a semicolon separated list of meanings in English */
  def meaning: String

  /** return a space separated list of readings in Japanese Kana */
  def reading: String

  // methods that return default values
  def frequency: Int = 0
  def grade: Grade = Grade.NoGrade
  def level: Level = Level.NoLevel
  def kyu: Kyu = Kyu.NoKyu
  def link: Option[Kanji] = None
  def oldNames: List[String] = Nil
  def newName: Option[String] = None
  def number: Int = 0
  def year: Int = 0
  def reason: JinmeiReason = JinmeiReason.NoJinmeiReason
  def linkedReadings: LinkedReadings = LinkedReadings.No
  // 'has' helper methods
  def hasFrequency: Boolean = frequency > 0
  def hasGrade: Boolean = grade.isDefined
  def hasLevel: Boolean = level.isDefined
  def hasKyu: Boolean = kyu.isDefined
  def hasLink: Boolean = link.isDefined
  def hasOldNames: Boolean = oldNames.nonEmpty
  def hasNewName: Boolean = newName.isDefined
  def hasNumber: Boolean = number > 0
  def hasYear: Boolean = year > 0
  def hasReason: Boolean = reason.isDefined

  protected def error(msg: String): Nothing =
    throw DomainException(getClass.getSimpleName + ": " + msg)
}

object Kanji {
  /** used by [[Kanji.info]] method to indicate what info to exclude */
  enum Info {
    case Frequency, Grade, Level, Kyu, NoInfo
  }

  // abstract subclasses of Kanji

  /** base class for the officially recognized variants stored in 'jouyou.txt' and 'jinmei.txt'.
   *  Some of these Kanji are in the top 2,501 frequency list and almost all of them are in Kentei
   *  KJ1 or K1. However, none of them have a JLPT level.
   */
  sealed abstract class Linked(name: String, radical: Radical, strokes: Int, l: Kanji,
      override val frequency: Int, override val kyu: Kyu)
  extends Kanji(name, radical, strokes) {
    override val link: Option[Kanji] = l.some
    override def meaning: String = l.meaning
    override def reading: String = l.reading
    override def linkedReadings: LinkedReadings = LinkedReadings.Yes
    override def newName: Option[String] = l.name.some
  }

  /** contains 'meaning' and 'reading' fields loaded from files */
  sealed abstract class Loaded(name: String, radical: Radical, strokes: Int,
      override val meaning: String, override val reading: String)
  extends Kanji(name, radical, strokes) {}

  // abstract subclasses of Loaded

  sealed abstract class Numbered(name: String, radical: Radical, strokes: Int, meaning: String,
      reading: String, override val kyu: Kyu, override val number: Int)
  extends Loaded(name, radical, strokes, meaning, reading) {
    if (number <= 0) error("number must be greater than zero")
  }

  sealed abstract class Other(name: String, radical: Radical, strokes: Int, meaning: String,
      reading: String, oldLinks: OldLinks, linkNames: List[String],
      override val linkedReadings: LinkedReadings)
  extends Loaded(name, radical, strokes, meaning, reading) {
    override val oldNames: List[String] = if (oldLinks) linkNames else Nil
    override val newName: Option[String] = if (oldLinks) None else linkNames.headOption
  }

  // abstract subclasses of Numbered

  sealed abstract class Official(name: String, radical: Radical, strokes: Int, meaning: String,
      reading: String, kyu: Kyu, number: Int, override val level: Level,
      override val frequency: Int, override val year: Int)
  extends Numbered(name, radical, strokes, meaning, reading, kyu, number) {}

  // abstract subclasses of Other

  sealed abstract class Standard(name: String, radical: Radical, strokes: Int, meaning: String,
      reading: String, oldLinks: OldLinks, linkNames: List[String], linkedReadings: LinkedReadings,
      override val kyu: Kyu)
  extends Other(name, radical, strokes, meaning, reading, oldLinks, linkNames, linkedReadings) {}
}

// concrete subclasses of Kanji.Numbered

final class ExtraKanji(name: String, radical: Radical, strokes: Int, meaning: String,
    reading: String, kyu: Kyu, number: Int, override val newName: Option[String] = None)
extends Numbered(name, radical, strokes, meaning, reading, kyu, number) {

  override def kanjiType: KanjiType = KanjiType.Extra
  override def suffix: Char = '+'
}

// concrete subclasses of Kanji.Official

final class JinmeiKanji(name: String, radical: Radical, strokes: Int, meaning: String,
    reading: String, kyu: Kyu, number: Int, level: Level, frequency: Int, year: Int,
    override val oldNames: List[String], override val reason: JinmeiReason)
extends Official(name, radical, strokes, meaning, reading, kyu, number, level, frequency, year) {
  // Jinmei Kanji have year values starting at 1951, but for now just ensure
  // it's non-zero to prevent the case of constructing without a year
  if (year == 0) error("must have a valid year")
  if (!reason.isDefined) error("must have a valid reason")

  override def kanjiType: KanjiType = KanjiType.Jinmei
  override def suffix: Char = if (hasLevel) '\'' else '^'
}

final class JouyouKanji(name: String, radical: Radical, strokes: Int, meaning: String,
    reading: String, kyu: Kyu, number: Int, level: Level, frequency: Int, year: Int,
    override val oldNames: List[String], override val grade: Grade)
extends Official(name, radical, strokes, meaning, reading, kyu, number, level, frequency, year) {
  if (!grade.isDefined) error("must have a valid grade")

  override def kanjiType: KanjiType = KanjiType.Jouyou
  override def suffix: Char = '.'
}

// concrete subclass of Kanji.Other

final class UcdKanji(name: String, radical: Radical, strokes: Int, meaning: String, reading: String,
    oldLinks: OldLinks, linkNames: List[String], linkedReadings: LinkedReadings)
extends Other(name, radical, strokes, meaning, reading, oldLinks, linkNames, linkedReadings) {

  override def kanjiType: KanjiType = KanjiType.Ucd
  override def suffix: Char = '*'
}

// concrete subclasses of Kanji.Standard

final class FrequencyKanji(name: String, radical: Radical, strokes: Int, meaning: String,
    reading: String, oldLinks: OldLinks, linkNames: List[String], linkedReadings: LinkedReadings,
    kyu: Kyu, override val frequency: Int)
extends Standard(
  name, radical, strokes, meaning, reading, oldLinks, linkNames, linkedReadings, kyu) {
  if (frequency <= 0) error("frequency must be greater than zero")

  override def kanjiType: KanjiType = KanjiType.Frequency
  override def suffix: Char = '"'
}

final class KenteiKanji(name: String, radical: Radical, strokes: Int, meaning: String,
    reading: String, oldLinks: OldLinks, linkNames: List[String], linkedReadings: LinkedReadings,
    kyu: Kyu)
extends Standard(
  name, radical, strokes, meaning, reading, oldLinks, linkNames, linkedReadings, kyu) {
  if (!kyu.isDefined) error("must have a valid kyu")

  override def kanjiType: KanjiType = KanjiType.Kentei
  override def suffix: Char = if (kyu == Kyu.K1) '#' else '@'
}

// concrete subclasses of Kanji.Linked

/** official set of 230 Jinmeiyō Kanji that are old or alternative forms of JouyouKanji or
 *  JinmeiKanji. There are 230 of these Kanji:
 *  <ul>
 *  <li>204 are part of the 365 JouyouKanji 'old names' set
 *  <li>8 are different alternate forms of JouyouKanji (薗 駈 嶋 盃 冨 峯 埜 凉)
 *  <li>18 are alternate forms of standard JinmeiKanji
 *  </ul>
 */
final class LinkedJinmeiKanji(name: String, radical: Radical, strokes: Int, link: Kanji,
    frequency: Int, kyu: Kyu)
extends Linked(name, radical, strokes, link, frequency, kyu) {
  if (!link.isInstanceOf[Official]) error("link must be JouyouKanji or JinmeiKanji")

  override def kanjiType: KanjiType = KanjiType.LinkedJinmei
  override def suffix: Char = '~'
}

/** 163 Kanji that link to a JouyouKanji. These are the published Jōyō variants that aren't
 *  already included in the 230 Jinmeiyō 'official variants'.
 */
final class LinkedOldKanji(name: String, radical: Radical, strokes: Int, link: Kanji,
    frequency: Int, kyu: Kyu)
extends Linked(name, radical, strokes, link, frequency, kyu) {
  if (!link.isInstanceOf[JouyouKanji]) error("link must be JouyouKanji")

  override def kanjiType: KanjiType = KanjiType.LinkedOld
  override def suffix: Char = '%'
}
