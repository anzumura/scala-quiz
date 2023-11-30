package quiz

import UnicodeUtils._

class UnicodeUtilsTest extends BaseTest {
  "code toString" in {
    assert(Code().toString == "U+0000")
    assert(Code(62).toString == "U+003E")
    assert(Code(0xfff).toString == "U+0FFF")
    assert(Code(0xffff).toString == "U+FFFF")
    assert(Code(0x10000).toString == "U+10000")
    assert(Code(UnicodeMax).toString == "U+10FFFF")
  }

  "code can't be negative" in {
    domainException(Code(-1), "code can't be negative")
  }

  "code 0x110000 exceeds unicode max U+10FFFF" in {
    domainException(Code(-1), "code can't be negative")
  }

  "create unicode block" in {
    val b = Block(Code(0), Code(255))
    assert(b.start.value == 0)
    assert(b.end.value == 255)
  }

  "create unicode block with only one entry" in {
    val b = Block(Code(256))
    assert(b.start.value == 256)
    assert(b.start == b.end)
  }

  "block end cannot be less than start" in {
    domainException(Block(Code(20), Code(19)),
      "end U+0013 is less than start U+0014")
  }
}
