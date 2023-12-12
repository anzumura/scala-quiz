package quiz.kanji

import quiz.kanji.Kanji.*
import quiz.utils.DomainException

import scala.language.implicitConversions

sealed abstract class Kanji(val name: String, val radical: Radical, val strokes: Int) {
  // abstract methods
  def kanjiType: KanjiType
  def meaning: String
  def reading: String
  // methods that return default values
  def link: Option[Kanji] = None
  def frequency: Int = 0
  def kyu: Kyu = Kyu.NoKyu
  def year: Int = 0
  def linkedReadings: LinkedReadings = LinkedReadings.No
  def oldNames: List[String] = Nil
  def newName: Option[String] = None
  def grade: Grade = Grade.NoGrade
  def level: Level = Level.NoLevel
  def number: Int = 0
  def reason: JinmeiReason = JinmeiReason.NoJinmeiReason

  protected def error(msg: String): Nothing =
    throw DomainException(getClass.getSimpleName + ": " + msg)
}

object Kanji {
  // abstract subclasses of Kanji

  /** base class for the officially recognized variants stored in 'jouyou.txt' and 'jinmei.txt'.
   *  Some of these Kanji are in the top 2,501 frequency list and almost all of them are in Kentei
   *  KJ1 or K1. However, none of them have a JLPT level.
   */
  sealed abstract class Linked(name: String, radical: Radical, strokes: Int, l: Kanji,
      override val frequency: Int, override val kyu: Kyu)
  extends Kanji(name, radical, strokes) {
    override val link: Option[Kanji] = Option(l)
    override def meaning: String = link.map(_.meaning).getOrElse("")
    override def reading: String = link.map(_.reading).getOrElse("")
    override def linkedReadings: LinkedReadings = LinkedReadings.Yes
    override def newName: Option[String] = link.map(_.name)
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
}

final class JouyouKanji(name: String, radical: Radical, strokes: Int, meaning: String,
    reading: String, kyu: Kyu, number: Int, level: Level, frequency: Int, year: Int,
    override val oldNames: List[String], override val grade: Grade)
extends Official(name, radical, strokes, meaning, reading, kyu, number, level, frequency, year) {
  if (!grade.isDefined) error("must have a valid grade")
  override def kanjiType: KanjiType = KanjiType.Jouyou
}

// concrete subclass of Kanji.Other

final class UcdKanji(name: String, radical: Radical, strokes: Int, meaning: String, reading: String,
    oldLinks: OldLinks, linkNames: List[String], linkedReadings: LinkedReadings)
extends Other(name, radical, strokes, meaning, reading, oldLinks, linkNames, linkedReadings) {
  override def kanjiType: KanjiType = KanjiType.Ucd
}

// concrete subclasses of Kanji.Standard

final class FrequencyKanji(name: String, radical: Radical, strokes: Int, meaning: String,
    reading: String, oldLinks: OldLinks, linkNames: List[String], linkedReadings: LinkedReadings,
    kyu: Kyu, override val frequency: Int)
extends Standard(
  name, radical, strokes, meaning, reading, oldLinks, linkNames, linkedReadings, kyu) {
  if (frequency <= 0) error("frequency must be greater than zero")
  override def kanjiType: KanjiType = KanjiType.Frequency
}

final class KenteiKanji(name: String, radical: Radical, strokes: Int, meaning: String,
    reading: String, oldLinks: OldLinks, linkNames: List[String], linkedReadings: LinkedReadings,
    kyu: Kyu)
extends Standard(
  name, radical, strokes, meaning, reading, oldLinks, linkNames, linkedReadings, kyu) {
  if (!kyu.isDefined) error("must have a valid kyu")
  override def kanjiType: KanjiType = KanjiType.Kentei
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
}

/** 163 Kanji that link to a JouyouKanji. These are the published Jōyō variants that aren't
 *  already included in the 230 Jinmeiyō 'official variants'.
 */
final class LinkedOldKanji(name: String, radical: Radical, strokes: Int, link: Kanji,
    frequency: Int, kyu: Kyu)
extends Linked(name, radical, strokes, link, frequency, kyu) {
  if (!link.isInstanceOf[JouyouKanji]) error("link must be JouyouKanji")
  override def kanjiType: KanjiType = KanjiType.LinkedOld
}
