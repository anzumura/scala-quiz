package quiz

object UnicodeUtils extends ThrowsDomainException {
  val UnicodeMax: Int = 0x10ffff

  final class Code private (val value: Int) extends AnyVal with Ordered[Code] {
    override def compare(rhs: Code): Int = value - rhs.value

    /**
     * @return standard Unicode code point representation, i.e., U+'hex value'
     */
    override def toString: String =
      (if (value <= 0xfff) "U+%04X" else "U+%X").format(value)

    /**
     * @return standard Java (UTF-16) String for this code point
     */
    def toUTF16: String = Character.toString(value)
  }

  object Code {
    /**
     * @param value string value to construct Unicode Code
     * @param sizeOne if true then string must contain only one Unicode 'letter'
     *                if false then the first Unicode letter is converted
     * @return unicode Code
     * @throws DomainException if value is empty or value contains more than one
     *                         Unicode letter when `sizeOne` is set to true
     */
    def apply(value: String, sizeOne: Boolean = true): Code = {
      if (value.isEmpty) error("cannot create Unicode Code from empty string")
      val x = value.codePointAt(0)
      if (sizeOne && Character.charCount(x) < value.length)
        error("value has more than one Unicode letter")
      new Code(x)
    }

    /**
     * @return a Unicode Code instance with a value of 'zero' (NUL)
     */
    def apply(): Code = new Code(0)

    /**
     * @param value integer value to use to create Unicode Code instance
     * @return Unicode Code instance
     * @throws DomainException is value is negative or exceeds Unicode Max
     */
    def apply(value: Int): Code = {
      if (value < 0) error("code can't be negative")
      if (value > UnicodeMax)
        error("code 0x%x exceeds Unicode max U+%X".format(value, UnicodeMax))
      new Code(value)
    }
  }

  case class Block(start: Code, end: Code) {
    if (end < start) error(s"end $end is less than start $start")

    def apply(x: Code): Boolean = start <= x && x <= end
  }

  object Block {
    def apply(start: Code): Block = new Block(start, start)
  }

  /**
   * @param x the Unicode code to test
   * @return true if `x` is in one of the common Kanji blocks
   * @see commonKanjiBlocks below (all blocks added by Unicode ver 3.1)
   */
  def isCommonKanji(x: Code): Boolean = exists(x, commonKanjiBlocks)

  /**
   * @param x the Unicode code to test
   * @return true if `x` is in one of the rare Kanji blocks
   * @see rareKanjiBlocks below (blocks added in Unicode ver 3.0 and later)
   */
  def isRareKanji(x: Code): Boolean = exists(x, rareKanjiBlocks)

  /**
   * @param x the Unicode code to test
   * @return true if `x` is in any Kanji block
   */
  def isKanji(x: Code): Boolean = isCommonKanji(x) || isRareKanji(x)

  /**
   * convenience method for checking if a string is a Kanji
   * @see Code.apply(String,Boolean)
   */
  def isKanji(x: String, sizeOne: Boolean = true): Boolean =
    isKanji(Code(x, sizeOne))

  private def exists(x: Code, blocks: Array[Block]) = {
    blocks.exists(_(x))
  }

  // used to create official Unicode Blocks (see below)
  private def block(start: Int, end: Int) = new Block(Code(start), Code(end))

  /**
   * blocks containing all common Japanese Kanji (plus many non-Japanese Kanji)
   * Most Japanese Kanji are in the original 'CJK Unified Ideographs' block.
   * 'Extension A' has one Kentei Kanji and 'Extension B' has an updated Jouyou
   * Kanji '𠮟' (U+20B9F) which used to be '叱' (U+53F1). The Compatibility block
   * contains many 'single grapheme versions of old/variant Kanji that used to
   * require two graphemes, i.e., a base character plus a variation selector.
  */
  private val commonKanjiBlocks = Array(
    block(0x3400, 0x4dbf), // CJK Extension A, ver 3.0 Sep 1999
    block(0x4e00, 0x9fff), // CJK Unified Ideographs, ver 1.1 Jun 1993
    block(0xf900, 0xfaff), // CJK Compat. Ideographs, ver 1.1 Jun 1993
    block(0x20000, 0x2a6df) // CJK Extension B, ver 3.1 Mar 2001
  )

  /**
   * blocks that contain rare Kanji. Extensions C, D, E and F are contiguous so
   * combine into one block (for efficient lookup). Here are the actual ranges:
   * U+2A700 to U+2B73F : CJK Extension C, ver 5.2 Oct 2009
   * U+2B740 to U+2B81F : CJK Extension D, ver 6.0 Oct 2010
   * U+2B820 to U+2CEAF : CJK Extension E, ver 8.0 Jun 2015
   * U+2CEB0 to U+2EBEF : CJK Extension F, ver 10.0 Jun 2016
  */
  private val rareKanjiBlocks = Array(
    block(0x2e80, 0x2eff), // Radicals Supp.
    block(0x2a700, 0x2ebef), // CJK Extension C-F
    block(0x2f800, 0x2fa1f), // CJK Compat. Supp.
    block(0x30000, 0x3134f) // CJK Extension G
  )
}
