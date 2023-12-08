package quiz.kanji

import quiz.utils.ThrowsDomainException
import quiz.utils.UnicodeUtils.Code

import java.nio.file.Path
import scala.collection.immutable.BitSet

class UcdData(path: Path) extends ThrowsDomainException {}

object UcdData {
  /** represents the XML property from which the link was loaded. '_R' means the
   *  link was also used to pull in readings. '_R' come first in the enum to
   *  allow '<' comparison to find all reading links. Note, there is no non '_R'
   *  type for 'Semantic' links by design.
   */
  enum LinkType {
    case Compatibility_R // *kCompatibilityVariant* link also used for 'reading'
    case Definition_R    // *kDefinition based* link and also used for 'reading'
    case Jinmei_R        // *kJinmeiyoKanji* link also used for 'reading'
    case Semantic_R      // *kSemanticVariant* link also used for 'reading'
    case Simplified_R    // *kSimplifiedVariant* link also used for 'reading'
    case Traditional_R   // *kTraditionalVariant* link also used for 'reading'
    case Compatibility   // *kCompatibilityVariant* link
    case Definition      // *kDefinition based* link
    case Jinmei          // *KJinmeiyoKanji* link
    case Simplified      // *kSimplifiedVariant* link
    case Traditional     // *kTraditionalVariant* link
    case None            // no link
  }

  /** set of sources for a Ucd entry plus info on whether the entry is a 'Joyo'
   *  or 'Jinmei' Kanji (officially an entry can't be both 'joyo' and 'jinmei',
   *  but these are distinct properties in the raw data so keep them separate
   *  here as well). The letters in `sources` represent 'country/region' where
   *  there's source data. The top six 'countries' are currently supported:
   *  <ul>
   *  <li>G: People’s Republic of China and Singapore
   *  <li>H: Hong Kong
   *  <li>J: Japan, i.e., 'kIRG_JSource' has a non-empty value
   *  <li>K: Republic of Korea (South Korea)
   *  <li>T: Taiwan
   *  <li>V: Vietnam
   *  </ul>
   */
  class Source private (val jSource: String, sources: String, joyo: Boolean,
      jinmei: Boolean) {
    import Source.Bits.*
    /** returns a string containing source country letters (can be empty) */
    override def toString: String = sourceString(bits)

    /** returns true if this Source is for a Jōyō Kanji */
    def isJoyo: Boolean = bits(Joyo.ordinal)

    /** returns true if this Source if for a Jinmeiyō Kanji */
    def isJinmei: Boolean = bits(Jinmei.ordinal)

    private val bits = bitSet(sources, joyo, jinmei)
  }

  private object Source {
    private enum Bits { case G, H, J, K, T, V, Joyo, Jinmei }
    private object Bits {
      private lazy val sources = for {
        v <- values
        x = v.ordinal
        if x < Joyo.ordinal
      } yield x

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
  }

  /** data for an entry from the "ucd.txt" file. Some columns in the file aren't
   *  stored like "Name" and "LinkNames" since these are represented by [[Code]]
   *  classes. Also ignore "VStrokes" (and maybe modify the parse script to stop
   *  calculating it). "On" and "Kun" columns used to be the only sources for
   *  Japanese (Rōmaji) readings, but the "kJapanese" field was added in Unicode
   *  15.1 that can be used instead (it's also populated with 'Kana' as well)
   *
   *  @param code Unicode code point
   *  @param radical official radical
   *  @param strokes total stroke count (mostly accurate, but not 100% perfect)
   *  @param pinyin 'hànyǔ pīnyīn' (漢語拼音) from 'kMandarin' XML property
   *  @param morohashiId see [[MorohashiId]]
   *  @param nelsonIds 'Classic Nelson' ids, see parse script for details
   *  @param source see [[Source]]
   *  @param links list of Unicode code points representing links
   *  @param linkType see [[LinkType]]
   *  @param meaning meaning of the character in English
   *  @param reading Japanese readings in 'Kana' (仮名)
   */
  case class Ucd(code: Code, radical: String, strokes: Int, pinyin: String,
      morohashiId: MorohashiId, nelsonIds: Set[Int], source: Source,
      links: List[Code], linkType: LinkType, meaning: String, reading: String)
}
