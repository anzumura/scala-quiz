package quiz.kanji

import quiz.utils.ThrowsDomainException
import quiz.utils.UnicodeUtils.Code

import java.nio.file.Path
import scala.collection.immutable.BitSet

class UcdData(path: Path) extends ThrowsDomainException {}

object UcdData {
  private enum Bits { case G, H, J, K, T, V, Joyo, Jinmei }
  private object Bits {
    private lazy val sources =
      values.filter(v => v != Joyo && v != Jinmei).map(_.ordinal)

    def bitSet(sources: String, joyo: Boolean, jinmei: Boolean): BitSet =
      sources.foldLeft(
        if (joyo) {
          if (jinmei) BitSet(Joyo.ordinal, Jinmei.ordinal)
          else BitSet(Joyo.ordinal)
        } else if (jinmei) BitSet(Jinmei.ordinal)
        else BitSet()
      )((bits, x) => bits + valueOf(x.toString).ordinal)

    def sourceString(bits: BitSet): String =
      sources.filter(bits).foldLeft("")(_ + _.toString)
  }

  class Sources private (sources: String, joyo: Boolean, jinmei: Boolean) {
    private val bits = Bits.bitSet(sources, joyo, jinmei)

    override def toString: String = Bits.sourceString(bits)

    def isJoyo: Boolean = bits(Bits.Joyo.ordinal)
    def isJinmei: Boolean = bits(Bits.Jinmei.ordinal)
  }

  case class Ucd(code: Code, radical: String, strokes: Int, pinyin: String,
      morohashiId: MorohashiId, nelsonIds: Set[Int])
}
