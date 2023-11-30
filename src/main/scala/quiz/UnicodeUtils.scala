package quiz

object UnicodeUtils extends ThrowsDomainException {
  val UnicodeMax: Int = 0x10ffff

  final class Code private (val value: Int) extends AnyVal with Ordered[Code] {
    override def compare(rhs: Code): Int = value - rhs.value
    override def toString: String =
      (if (value <= 0xfff) "U+%04X" else "U+%X").format(value)
  }

  object Code {
    def apply() : Code = new Code(0)
    def apply(value: Int): Code = {
      if (value < 0) error("code can't be negative")
      if (value > UnicodeMax)
        error("code 0x%x exceeds unicode max %X".format(value, UnicodeMax))
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
}
