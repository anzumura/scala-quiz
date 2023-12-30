package quiz.utils

import quiz.utils.Block.{CommonKanjiBlocks, RareKanjiBlocks}

import scala.util.{Success, Try}

final class Code private (val value: Int) extends AnyVal with Ordered[Code]:
  override def compare(rhs: Code): Int = value - rhs.value

  /** returns standard Unicode code point format, i.e., U+'hex value' */
  override def toString: String = (if value <= 0xfff then "U+%04X" else "U+%X").format(value)

  /** returns standard Java (UTF-16) String for this code point */
  def toUTF16: String = Character.toString(value)

  /** returns true if this Code is in one of the common Kanji blocks
   *  @see Block.commonKanjiBlocks (all blocks added by Unicode ver 3.1)
   */
  def isCommonKanji: Boolean = exists(CommonKanjiBlocks)

  /** returns true if this Code is in one of the rare Kanji blocks
   *  @see Block.rareKanjiBlocks (blocks added in Unicode ver 3.0 and later)
   */
  def isRareKanji: Boolean = exists(RareKanjiBlocks)

  /** returns true if this Code is in any Kanji block */
  def isKanji: Boolean = isCommonKanji || isRareKanji

  private def exists(blocks: Array[Block]) = blocks.exists(_(this))

object Code extends ThrowsDomainException:
  val UnicodeMax: Int = 0x10ffff

  /** @param s string value to construct Unicode Code
   *  @param sizeOne if true then string can contain only one Unicode 'letter'
   *                 if false then the first Unicode letter is converted
   *  @return unicode Code
   *  @throws DomainException if `value` is empty or value contains more than
   *                          one Unicode letter when `sizeOne` is set to true
   */
  def apply(s: String, sizeOne: Boolean = true): Code =
    checkEmpty(s)
    val x = s.codePointAt(0)
    if sizeOne && Character.charCount(x) < s.length then
      error(s"'$s' has more than one Unicode letter")
    new Code(x)

  /** returns a Unicode Code instance with a value of 'zero' (NUL) */
  def apply(): Code = new Code(0)

  /** @param value integer value to use to create Unicode Code instance
   *  @return Unicode Code instance
   *  @throws DomainException if value is negative or exceeds Unicode Max
   */
  def apply(value: Int): Code =
    if value < 0 then error("code can't be negative")
    if value > UnicodeMax then error(f"code 0x$value%x exceeds Unicode max U+$UnicodeMax%X")
    new Code(value)

  def fromHex(s: String): Code =
    checkEmpty(s)
    Try(Integer.parseInt(s, 16)) match
      case Success(x) => apply(x)
      case _ => error(s"'$s' is not a valid hex string")

  private def checkEmpty(s: String): Unit =
    if s.isEmpty then error("cannot create Unicode Code from empty string")
