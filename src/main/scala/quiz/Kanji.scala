package quiz

import Kanji._

sealed abstract class Kanji(private val f: Fields) {
  def name: String = f.name
  def radical: String = f.radical
  def strokes: Int = f.strokes

  // abstract methods
  def kanjiType: Type.Value

  def meaning: String

  def reading: String

  // methods that return default values
  def link: Option[Kanji] = Option.empty[Kanji]

  def frequency: Int = 0

  def kyu: Kyu.Value = Kyu.None

  def year: Int = 0

  def hasLinkedReadings: Boolean = false

  def oldNames: List[String] = List.empty[String]

  def newName: Option[String] = Option.empty[String]

  def grade: Grade.Value = Grade.None

  def level: Level.Value = Level.None

  def number: Int = 0

  def reason: JinmeiReason.Value = JinmeiReason.None
}

object Kanji {
  /**
   * used to identify which official group (Jouyou or Jinmei) a Kanji belongs to
   * (or has a link to) as well as a few more groups for less common Kanji
   *
   * The current types are:
   * Jouyou: 2,136 official Jōyō (常用) Kanji
   * Jinmei: 633 official Jinmeiyō (人名用) Kanji
   * LinkedJinmei: 230 old/variant forms of Jouyou (212) and Jinmei 18)
   * LinkedOld: 163 old/variant Jouyou Kanji that aren't in LinkedJinmei
   * Frequency: 124 from 'frequency.txt' that aren't one of the above types
   * Extra: loaded from 'extra.txt' (file doesn't contain above types)
   * Kentei: loaded from 'kentei/' directory and not one of the above types
   * Ucd: loaded from 'ucd.txt' and not one of the above types
   */
  object Type extends Enumeration {
    type Type = Value
    val Jouyou, Jinmei, LinkedJinmei, LinkedOld, Frequency, Extra, Kentei, Ucd
    = Value
  }

  /**
   * represents the official school grade for all Jouyou Kanji
   */
  object Grade extends Enumeration {
    type Grade = Value
    val G1, G2, G3, G4, G5, G6, S, None = Value
  }

  /**
   * JLPT (Japanese Language Proficiency Test) Levels covers 2,222 total Kanji
   * (including 1,971 Jouyou and 251 Jinmei)
   */
  object Level extends Enumeration {
    type Level = Value
    val N5, N4, N3, N2, N1, None = Value
  }

  /**
   * Kanji Kentei (漢字検定) Kyū (級), K = Kanken (漢検), J=Jun (準)
   *
   * @see <a href="https://en.wikipedia.org/wiki/Kanji_Kentei"></a>
   */
  object Kyu extends Enumeration {
    type Kyu = Value
    val K10, K9, K8, K7, K6, K5, K4, K3, KJ2, K2, KJ1, K1, None = Value
  }

  /**
   * official reason Kanji was added to Jinmeiyō list:
   * Names: 246 Kanji - for use in names
   * Print: 352 Kanji - for use in publications
   * Variant: 2 Kanji - allowed variant form (異体字)
   * Moved: 5 Kanji - moved out of Jouyou into Jinmei
   * Simple: 2 Kanji - simplified (表外漢字字体表の簡易慣用字体)
   * Other: 26 Kanji - reason listed as その他
   * None: not a Jinmei type Kanji
   */
  object JinmeiReason extends Enumeration {
    type JinmeiReason = Value
    val Names, Print, Variant, Moved, Simple, Other, None = Value
  }

  /**
   * common fields for all Kanji classes
   */
  case class Fields(name: String, radical: String, strokes: Int)

  /**
   * additional fields for Linked Kanji classes
   */
  case class LinkedFields(link: Kanji, frequency: Int, kyu: Kyu.Value)

  /**
   * additional fields for Loaded Kanji classes
   */
  case class LoadedFields(meaning: String, reading: String)

  /**
   * base class for the officially recognized variants stored in 'jouyou.txt'
   * and 'jinmei.txt'. Some of these Kanji are in the top 2,501 frequency list
   * and almost all of them are in Kentei KJ1 or K1. However, none of them have
   * a JLPT level.
   */
  sealed abstract class Linked(f: Fields, private val lf: LinkedFields) extends
    Kanji(f) {
    override def link: Option[Kanji] = Option(lf.link)
    override def frequency: Int = lf.frequency
    override def kyu: Kyu.Value = lf.kyu

    override def meaning: String = lf.link.meaning

    override def reading: String = lf.link.meaning

    override def hasLinkedReadings = true

    override def newName: Option[String] = link.map(_.name)
  }

  /**
   * contains 'meaning' and 'reading' fields loaded from files
   */
  sealed abstract class Loaded(f: Fields, private val lf: LoadedFields) extends
    Kanji(f) {
    override def meaning: String = lf.meaning
    override def reading: String = lf.reading
  }
}

/**
 * official set of 230 Jinmeiyō Kanji that are old or alternative forms of
 * JouyouKanji or JinmeiKanji. There are 230 of these Kanji:
 * <ul>
 * <li>204 are part of the 365 JouyouKanji 'old names' set
 * <li>8 are different alternate forms of JouyouKanji (薗 駈 嶋 盃 冨 峯 埜 凉)
 * <li> 18 are alternate forms of standard JinmeiKanji
 * </ul>
 */
final class LinkedJinmeiKanji(name: String, radical: String, strokes: Int,
                              link: Kanji, frequency: Int, kyu: Kyu.Value)
  extends Linked(Fields(name, radical, strokes),
    LinkedFields(link, frequency, kyu)) {
  override def kanjiType: Type.Value = Type.LinkedJinmei
}

/**
 * 163 Kanji that link to a JouyouKanji. These are the published Jōyō variants
 * that aren't already included in the 230 Jinmeiyō 'official variants'.
 */
final class LinkedOldKanji(name: String, radical: String, strokes: Int,
                           link: Kanji, frequency: Int, kyu: Kyu.Value) extends
  Linked(Kanji.Fields(name, radical, strokes), LinkedFields(link,
    frequency, kyu)) {
  override def kanjiType: Type.Value = Type.LinkedOld
}