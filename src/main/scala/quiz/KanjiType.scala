package quiz

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
 * Ucd: loaded from 'ucd.txt' and not one of the above types */
object KanjiType extends Enumeration {
  type KanjiType = Value
  val Jouyou, Jinmei, LinkedJinmei, LinkedOld, Frequency, Extra, Kentei, Ucd
  = Value
}

/**
 * represents the official school grade for all Jouyou Kanji */
object Grade extends Enumeration {
  type Grade = Value
  val G1, G2, G3, G4, G5, G6, S, None = Value
}

/**
 * JLPT (Japanese Language Proficiency Test) Levels covers 2,222 total Kanji
 * (including 1,971 Jouyou and 251 Jinmei) */
object Level extends Enumeration {
  type Level = Value
  val N5, N4, N3, N2, N1, None = Value
}

/**
 * Kanji Kentei (漢字検定) Kyū (級), K = Kanken (漢検), J=Jun (準)
 *
 * @see <a href="https://en.wikipedia.org/wiki/Kanji_Kentei"></a> */
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
 * None: not a Jinmei type Kanji */
object JinmeiReason extends Enumeration {
  type JinmeiReason = Value
  val Names, Print, Variant, Moved, Simple, Other, None = Value
}