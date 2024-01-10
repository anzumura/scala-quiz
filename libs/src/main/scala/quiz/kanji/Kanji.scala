package quiz.kanji

import cats.syntax.all.*
import quiz.kanji.Kanji.*
import quiz.utils.DomainException

import scala.language.implicitConversions

sealed abstract class Kanji(val name: String, val radical: Radical, val strokes: Int, val ucd: Ucd):
  override def toString: String = name
  override def equals(obj: Any): Boolean =
    obj match
      case k: Kanji => name == k.name
      case _ => false

  /** @param exclude info to exclude from the result
   *  @return string containing info about this instance
   */
  def info(exclude: Info = Info.NoInfo): String =
    import Info.*
    val result = StringBuilder(s"$name$suffix Rad $radical, Strokes $strokes")
    if exclude != Frequency && hasFrequency then result ++= s", Frq $frequency"
    if exclude != Grade && hasGrade then result ++= s", $grade"
    if exclude != Level && hasLevel then result ++= s", $level"
    if exclude != Kyu && hasKyu then result ++= s", $kyu"
    result.result()

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

  protected def this(name: String, ucd: Ucd) = this(name, ucd.radical, ucd.strokes, ucd)

  protected def error(msg: String): Nothing =
    throw DomainException(getClass.getSimpleName + ": " + msg)

object Kanji:
  /** used during Kanji creation to help simplify the derived class constructors */
  final class Params(data: BaseKanjiData, val name: String, val ucd: Ucd):
    def this(data: BaseKanjiData, name: String) = this(data, name, data.getUcd(name))
    // get values from 'data'
    def frequency: Int = data.frequency(name)
    def level: Level = data.level(name)
    def kyu: Kyu = data.kyu(name)
    def radical(r: String): Radical = data.getRadical(r)
    // get values from 'ucd'
    def radical: Radical = ucd.radical
    def strokes: Int = ucd.strokes
    def meaning: String = ucd.meaning
    def reading: String = ucd.reading
    def oldNames: List[String] = if ucd.oldLinks then ucd.linkNames else Nil
    def newName: Option[String] = if ucd.oldLinks then None else ucd.linkNames.headOption

  /** used by [[Kanji.info]] method to indicate what info to exclude */
  enum Info:
    case Frequency, Grade, Level, Kyu, NoInfo

  // abstract subclasses of Kanji

  /** base class for the officially recognized variants stored in 'jouyou.txt' and 'jinmei.txt'.
   *  Some of these Kanji are in the top 2,501 frequency list and almost all of them are in Kentei
   *  KJ1 or K1. However, none of them have a JLPT level.
   */
  sealed abstract class Linked(params: Params, l: Kanji) extends Kanji(params.name, params.ucd):
    override val frequency: Int = params.frequency
    override val kyu: Kyu = params.kyu
    override val link: Option[Kanji] = l.some
    override def meaning: String = l.meaning
    override def reading: String = l.reading
    override def linkedReadings: LinkedReadings = LinkedReadings.Yes
    override def newName: Option[String] = l.name.some

  /** contains 'meaning' and 'reading' fields loaded from files */
  sealed abstract class Loaded(params: Params, radical: Radical, strokes: Int,
      override val meaning: String, override val reading: String)
  extends Kanji(params.name, radical, strokes, params.ucd) {}

  // abstract subclasses of Loaded

  sealed abstract class Numbered(params: Params, radical: String, strokes: Int, meaning: String,
      reading: String, override val number: Int)
  extends Loaded(params, params.radical(radical), strokes, meaning, reading):
    if number <= 0 then error("number must be greater than zero")
    override val kyu: Kyu = params.kyu

  sealed abstract class Other(params: Params)
  extends Loaded(params, params.radical, params.strokes, params.meaning, params.reading):
    override val linkedReadings: LinkedReadings = params.ucd.linkedReadings
    override val oldNames: List[String] = params.oldNames
    override val newName: Option[String] = params.newName

  // abstract subclasses of Numbered

  sealed abstract class Official(params: Params, radical: String, strokes: Int, meaning: String,
      reading: String, number: Int, override val year: Int)
  extends Numbered(params, radical, strokes, meaning, reading, number):
    override val frequency: Int = params.frequency
    override val level: Level = params.level

  // abstract subclasses of Other

  sealed abstract class Standard(params: Params, override val kyu: Kyu) extends Other(params):
    def this(params: Params) = this(params, params.kyu)

// concrete subclasses of Kanji.Numbered

final class ExtraKanji(params: Params, radical: String, strokes: Int, meaning: String,
    reading: String, number: Int)
extends Numbered(params, radical, strokes, meaning, reading, number):
  override def kanjiType: KanjiType = KanjiType.Extra
  override def suffix: Char = '+'

// concrete subclasses of Kanji.Official

final class JinmeiKanji(params: Params, radical: String, reading: String, number: Int, year: Int,
    override val oldNames: List[String], override val reason: JinmeiReason)
extends Official(params, radical, params.strokes, params.meaning, reading, number, year):
  // Jinmei Kanji have year values starting at 1951, but for now just ensure
  // it's non-zero to prevent the case of constructing without a year
  if year == 0 then error("must have a valid year")
  if !reason.isDefined then error("must have a valid reason")
  override def kanjiType: KanjiType = KanjiType.Jinmei
  override def suffix: Char = if hasLevel then '\'' else '^'

final class JouyouKanji(params: Params, radical: String, strokes: Int, meaning: String,
    reading: String, number: Int, year: Int, override val oldNames: List[String],
    override val grade: Grade)
extends Official(params, radical, strokes, meaning, reading, number, year):
  if !grade.isDefined then error("must have a valid grade")
  override def kanjiType: KanjiType = KanjiType.Jouyou
  override def suffix: Char = '.'

// concrete subclass of Kanji.Other

final class UcdKanji(params: Params) extends Other(params):
  override def kanjiType: KanjiType = KanjiType.Ucd
  override def suffix: Char = '*'

// concrete subclasses of Kanji.Standard

final class FrequencyKanji(params: Params, override val frequency: Int) extends Standard(params):
  if frequency <= 0 then error("frequency must be greater than zero")
  override def kanjiType: KanjiType = KanjiType.Frequency
  override def suffix: Char = '"'

final class KenteiKanji(params: Params, kyu: Kyu) extends Standard(params, kyu):
  if !kyu.isDefined then error("must have a valid kyu")
  override def kanjiType: KanjiType = KanjiType.Kentei
  override def suffix: Char = if kyu == Kyu.K1 then '#' else '@'

// concrete subclasses of Kanji.Linked

/** official set of 230 Jinmeiyō Kanji that are old or alternative forms of JouyouKanji or
 *  JinmeiKanji. There are 230 of these Kanji:
 *  <ul>
 *  <li>204 are part of the 365 JouyouKanji 'old names' set
 *  <li>8 are different alternate forms of JouyouKanji (薗 駈 嶋 盃 冨 峯 埜 凉)
 *  <li>18 are alternate forms of standard JinmeiKanji
 *  </ul>
 */
final class LinkedJinmeiKanji(params: Params, link: Kanji) extends Linked(params, link):
  link match
    case _: Official =>
    case _ => error("link must be JouyouKanji or JinmeiKanji")
  override def kanjiType: KanjiType = KanjiType.LinkedJinmei
  override def suffix: Char = '~'

/** 163 Kanji that link to a JouyouKanji. These are the published Jōyō variants that aren't
 *  already included in the 230 Jinmeiyō 'official variants'.
 */
final class LinkedOldKanji(params: Params, link: Kanji) extends Linked(params, link):
  link match
    case _: JouyouKanji =>
    case _ => error("link must be JouyouKanji")
  override def kanjiType: KanjiType = KanjiType.LinkedOld
  override def suffix: Char = '%'
