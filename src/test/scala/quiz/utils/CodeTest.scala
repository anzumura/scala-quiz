package quiz.utils

import quiz.test.BaseTest
import quiz.utils.Code.UnicodeMax

class CodeTest extends BaseTest {
  private val dog = Code(0x72ac)    // Kanji in the Basic Multilingual Plane
  private val scold = Code(0x20b9f) // Kanji in a Supplementary Plane

  "toString returns the standard Unicode representation" in {
    assert(Code().toString == "U+0000")
    assert(Code(62).toString == "U+003E")
    assert(Code(0xfff).toString == "U+0FFF")
    assert(Code(0xffff).toString == "U+FFFF")
    assert(Code(0x10000).toString == "U+10000")
    assert(Code(UnicodeMax).toString == "U+10FFFF")
  }

  "toUTF16 returns the code point as a standard Java (UTF-16) String" in {
    val result = dog.toUTF16
    assert(result == "犬")
    assert(result.length == 1)
  }

  "toUTF16 works for codes outside of BMP (Basic Multilingual Plane)" in {
    val result = scold.toUTF16
    assert(result == "𠮟")
    assert(result.length == 2)
  }

  "can't be negative" in { error(Code(-1), "code can't be negative") }

  "exceeds Unicode max U+10FFFF" in {
    val x = 0x110000
    error(Code(x), f"code 0x$x%x exceeds Unicode max U+10FFFF")
  }

  "create from string value" in {
    assert(Code("犬").value == dog.value)
    assert(Code("𠮟").value == scold.value)
  }

  "create from longer string value" in { assert(Code("犬猫", sizeOne = false).value == 0x72ac) }

  "can't create from empty string" in {
    error(Code(""), "cannot create Unicode Code from empty string")
  }

  "can't create from longer string is sizeOne is true (the default)" in {
    error(Code("犬猫"), "'犬猫' has more than one Unicode letter")
  }

  "create from hex value" in {
    val c = Code.fromHex("72ac")
    assert(c.toString == "U+72AC")
    assert(c.toUTF16 == "犬")
  }

  "can't create from empty hex" in {
    error(Code.fromHex(""), "cannot create Unicode Code from empty string")
  }

  "can't create from invalid hex" in {
    error(Code.fromHex("72ag"), "'72ag' is not a valid hex string")
    error(Code.fromHex("72ag"), "'72ag' is not a valid hex string")
  }

  "can't create from out-of-range hex" in {
    error(Code.fromHex("-1"), "code can't be negative")
    error(Code.fromHex("110000"), "code 0x110000 exceeds Unicode max U+10FFFF")
  }

  "create Unicode block" in {
    val b = Block(Code(0), Code(255))
    assert(b.start.value == 0)
    assert(b.end.value == 255)
  }
}
