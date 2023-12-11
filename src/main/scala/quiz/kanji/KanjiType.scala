package quiz.kanji

/** used to identify which official group (Jouyou or Jinmei) a Kanji belongs to (or has a link
 *  to) as well as a few more groups for less common Kanji
 *
 *  The current types are:
 *  <ul>
 *  <li>Jouyou: 2,136 official Jōyō (常用) Kanji
 *  <li>Jinmei: 633 official Jinmeiyō (人名用) Kanji
 *  <li>LinkedJinmei: 230 old/variant forms of Jouyou (212) and Jinmei 18)
 *  <li>LinkedOld: 163 old/variant Jouyou Kanji that aren't in LinkedJinmei
 *  <li>Frequency: 124 from 'frequency.txt' that aren't one of the above types
 *  <li>Extra: loaded from 'extra.txt' (file doesn't contain above types)
 *  <li>Kentei: loaded from 'kentei/' directory and not one of the above types
 *  <li>Ucd: loaded from 'ucd.txt' and not one of the above types
 *  </ul>
 */
enum KanjiType {
  case Jouyou, Jinmei, LinkedJinmei, LinkedOld, Frequency, Extra, Kentei, Ucd
}

/** represents the official school grade for all Jouyou Kanji */
enum Grade extends NoneEnum[Grade](Grade) {
  case G1, G2, G3, G4, G5, G6, S, None
}
object Grade extends NoneEnumObject[Grade] {}

/** JLPT (Japanese Language Proficiency Test) Levels covers 2,222 total Kanji (including 1,971
 *  Jouyou and 251 Jinmei)
 */
enum Level extends NoneEnum[Level](Level) {
  case N5, N4, N3, N2, N1, None
}
object Level extends NoneEnumObject[Level] {}

/** Kanji Kentei (漢字検定) Kyū (級), K = Kanken (漢検), J=Jun (準)
 *  @see <a href="https://en.wikipedia.org/wiki/Kanji_Kentei"></a>
 */
enum Kyu extends NoneEnum[Kyu](Kyu) {
  case K10, K9, K8, K7, K6, K5, K4, K3, KJ2, K2, KJ1, K1, None
}
object Kyu extends NoneEnumObject[Kyu] {}

/** official reason Kanji was added to Jinmeiyō list:
 *  <ul>
 *  <li>Names: 246 Kanji - for use in names
 *  <li>Print: 352 Kanji - for use in publications
 *  <li>Variant: 2 Kanji - allowed variant form (異体字)
 *  <li>Moved: 5 Kanji - moved out of Jouyou into Jinmei
 *  <li>Simple: 2 Kanji - simplified (表外漢字字体表の簡易慣用字体)
 *  <li>Other: 26 Kanji - reason listed as その他
 *  <li>None: not a Jinmei type Kanji
 *  </ul>
 */
enum JinmeiReason extends NoneEnum[JinmeiReason](JinmeiReason) {
  case Names, Print, Variant, Moved, Simple, Other, None
}
object JinmeiReason extends NoneEnumObject[JinmeiReason]

enum LinkedReadings {
  case Yes, No
}
object LinkedReadings {
  def apply(x: Boolean): LinkedReadings = if (x) LinkedReadings.Yes else LinkedReadings.No
  given Conversion[LinkedReadings, Boolean] = (x: LinkedReadings) => x eq LinkedReadings.Yes
}

enum OldLinks {
  case Yes, No
}
object OldLinks {
  def apply(x: Boolean): OldLinks = if (x) OldLinks.Yes else OldLinks.No
  given Conversion[OldLinks, Boolean] = (x: OldLinks) => x eq OldLinks.Yes
}
