package quiz.utils

import quiz.utils.Block.{CommonKanjiBlocks, RareKanjiBlocks}

import scala.util.{Success, Try}

opaque type Code = Int

final case class Block(start: Code, end: Code) extends ThrowsDomainException:
  if end < start then error(s"end ${end.toUnicode} is less than start ${start.toUnicode}")
  inline def apply(x: Code): Boolean = start <= x && x <= end

object Block:
  /** create a block with a single entry of `start` */
  def apply(start: Code): Block = Block(start, start)

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

given Ordering[Code] with
  inline def compare(x: Code, y: Code): Int = x - y

object Code extends ThrowsDomainException:
  export math.Ordering.Implicits.infixOrderingOps

  val UnicodeMax: Int = 0x10ffff

  /** @param s      string value to construct Unicode Code
   *  @param sizeOne if true then string can contain only one Unicode 'letter'
   *                if false then the first Unicode letter is converted
   *  @return unicode Code
   *  @throws DomainException if `value` is empty or value contains more than
   *                         one Unicode letter when `sizeOne` is set to true
   */
  def apply(s: String, sizeOne: Boolean = true): Code =
    checkEmpty(s)
    val x = s.codePointAt(0)
    if sizeOne && Character.charCount(x) < s.length then
      error(s"'$s' has more than one Unicode letter")
    x

  /** returns a Unicode Code instance with a value of 'zero' (NUL) */
  def apply(): Code = 0

  /** @param value integer value to use to create Unicode Code instance
   *  @return Unicode Code instance
   *  @throws DomainException if value is negative or exceeds Unicode Max
   */
  def apply(value: Int): Code =
    if value < 0 then error("code can't be negative")
    if value > UnicodeMax then error(f"code 0x$value%x exceeds Unicode max U+$UnicodeMax%X")
    value

  def fromHex(s: String): Code =
    checkEmpty(s)
    Try(Integer.parseInt(s, 16)) match
      case Success(x) => apply(x)
      case _ => error(s"'$s' is not a valid hex string")

  inline private def checkEmpty(s: String): Unit =
    if s.isEmpty then error("cannot create Unicode Code from empty string")

  /** convenience method for checking if a string is a Kanji
   *  @see Code.apply(String,Boolean)
   */
  def isKanji(x: String, sizeOne: Boolean = true): Boolean = Code(x, sizeOne).isKanji

  /** similar to `isKanji`, but returns false instead of throwing an exception if `x` is too long */
  def isOneKanji(x: String): Boolean = Try(isKanji(x)).getOrElse(false)

  extension (x: Code)
    /** returns standard Unicode code point format, i.e., U+'hex value' */
    def toUnicode: String = (if x <= 0xfff then "U+%04X" else "U+%X").format(x)

    /** returns standard Java (UTF-16) String for this code point */
    def toUTF16: String = Character.toString(x)

    /** returns true if this Code is in one of the common Kanji blocks
     *
     *  @see Block.commonKanjiBlocks (all blocks added by Unicode ver 3.1)
     */
    def isCommonKanji: Boolean = exists(CommonKanjiBlocks)

    /** returns true if this Code is in one of the rare Kanji blocks
     *
     *  @see Block.rareKanjiBlocks (blocks added in Unicode ver 3.0 and later)
     */
    def isRareKanji: Boolean = exists(RareKanjiBlocks)

    /** returns true if this Code is in any Kanji block */
    def isKanji: Boolean = isCommonKanji || isRareKanji

    inline private def exists(blocks: Array[Block]) = blocks.exists(_(x))
