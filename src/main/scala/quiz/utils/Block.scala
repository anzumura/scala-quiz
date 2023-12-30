package quiz.utils

import scala.util.Try

case class Block(start: Code, end: Code) extends ThrowsDomainException:
  if end < start then error(s"end $end is less than start $start")
  def apply(x: Code): Boolean = start <= x && x <= end

object Block:
  /** create a block with a single entry of `start` */
  def apply(start: Code): Block = Block(start, start)

  /** convenience method for checking if a string is a Kanji
   *  @see Code.apply(String,Boolean)
   */
  def isKanji(x: String, sizeOne: Boolean = true): Boolean = Code(x, sizeOne).isKanji

  /** similar to `isKanji`, but returns false instead of throwing an exception if `x` is too long */
  def isOneKanji(x: String): Boolean = Try(isKanji(x)).getOrElse(false)

  // used to create official Unicode Blocks (see below)
  private def block(start: Int, end: Int) = Block(Code(start), Code(end))

  /** blocks containing all common Japanese Kanji (plus many non-Japanese Kanji)
   *  Most Japanese Kanji are in 'CJK Unified Ideographs', but 'Extension A' has
   *  a Kentei Kanji and 'Extension B' has a Jouyou Kanji '𠮟' (U+20B9F) which
   *  used to be '叱' (U+53F1). 'Compatibility' contains many 'single grapheme'
   *  versions of old/variant Kanji that used to require two graphemes, i.e., a
   *  base character plus a variation selector.
   */
  val CommonKanjiBlocks: Array[Block] = Array(
    block(0x3400, 0x4dbf),  // CJK Extension A, ver 3.0 Sep 1999
    block(0x4e00, 0x9fff),  // CJK Unified Ideographs, ver 1.1 Jun 1993
    block(0xf900, 0xfaff),  // CJK Compat. Ideographs, ver 1.1 Jun 1993
    block(0x20000, 0x2a6df) // CJK Extension B, ver 3.1 Mar 2001
  )

  /** blocks that contain rare Kanji. Extensions C, D, E and F are contiguous so
   *  combine into one block (for efficient lookup). Here are the actual ranges:
   *  <ul>
   *  <li>U+2A700 to U+2B73F : CJK Extension C, ver 5.2 Oct 2009
   *  <li>U+2B740 to U+2B81F : CJK Extension D, ver 6.0 Oct 2010
   *  <li>U+2B820 to U+2CEAF : CJK Extension E, ver 8.0 Jun 2015
   *  <li>U+2CEB0 to U+2EBEF : CJK Extension F, ver 10.0 Jun 2016
   *  </ul>
   */
  val RareKanjiBlocks: Array[Block] = Array(
    block(0x2e80, 0x2eff),   // Radicals Supp.
    block(0x2a700, 0x2ebef), // CJK Extension C-F
    block(0x2f800, 0x2fa1f), // CJK Compat. Supp.
    block(0x30000, 0x3134f)  // CJK Extension G
  )
