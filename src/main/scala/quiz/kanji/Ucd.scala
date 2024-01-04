package quiz.kanji

import quiz.kanji.Ucd.{LinkType, Sources}
import quiz.utils.{Code, NoValueEnum, NoValueEnumObject, ThrowsDomainException}

import scala.collection.immutable.BitSet
import scala.util.Try

/** data for an entry from the "ucd.txt" file. Some columns in the file aren't stored like
 *  "Name" and "LinkNames" since these are represented by [[Code]] classes. Also ignore
 *  "VStrokes" (and maybe modify the parse script to stop calculating it). "On" and "Kun"
 *  columns used to be the only sources for Japanese (Rōmaji) readings, but the "kJapanese"
 *  field was added in Unicode 15.1 that can be used instead (it's also populated with 'Kana')
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
case class Ucd(code: Code, radical: Radical, strokes: Int, pinyin: String,
    morohashiId: Option[MorohashiId], nelsonIds: List[Int], source: Sources, links: List[Code],
    linkType: LinkType, meaning: String, reading: String)
extends ThrowsDomainException:
  // ensure there are links if LinkType not set to NoLinkType
  if linkType.isDefined then
    if links.isEmpty then error("LinkType without links")
  else if links.nonEmpty then error("links without LinkType")
  // convert names to standard strings
  def name: String = code.toUTF16
  def linkNames: List[String] = links.map(_.toUTF16)
  // get values from `sources`
  def jSource: String = source.jSource
  def joyo: Boolean = source.isJoyo
  def jinmei: Boolean = source.isJinmei
  // helper methods for working with links
  import LinkType.*
  def linkedReadings: LinkedReadings = LinkedReadings(linkType < Compatibility)
  def oldLinks: OldLinks = OldLinks(linkType == Traditional || linkType == Traditional_R)
  def linkedJinmei: Option[Code] = links.headOption
    .filter(_ => linkType == Jinmei || linkType == Jinmei_R)

object Ucd:
  /** represents the XML property from which the link was loaded. '_R' means the link was also
   *  used to pull in readings. '_R' come first in the enum to allow '<' comparison to find all
   *  reading links. Note, there is no non '_R' type for 'Semantic' links by design.
   */
  enum LinkType extends NoValueEnum[LinkType](LinkType):
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
    case NoLinkType      // no link
  object LinkType extends NoValueEnumObject[LinkType]

  /** set of sources for a Ucd entry plus info on whether the entry is a 'Joyo' or 'Jinmei' Kanji
   *  (officially an entry can't be both 'joyo' and 'jinmei', but these are distinct properties
   *  in the raw data so keep them separate here as well). The letters in `sources` represent
   *  'country/region' where there's source data. The top six 'countries' are currently supported:
   *  <ul>
   *  <li>G: People’s Republic of China and Singapore
   *  <li>H: Hong Kong
   *  <li>J: Japan, i.e., 'kIRG_JSource' has a non-empty value
   *  <li>K: Republic of Korea (South Korea)
   *  <li>T: Taiwan
   *  <li>V: Vietnam
   *  </ul>
   */
  class Sources(val jSource: String, sources: String, joyo: Boolean, jinmei: Boolean):
    import Sources.Bits.*
    /** returns a string containing source country letters (can be empty) */
    override def toString: String = sourceString(bits)

    /** returns true if this Source is for a Jōyō Kanji */
    def isJoyo: Boolean = bits(Joyo.ordinal)

    /** returns true if this Source if for a Jinmeiyō Kanji */
    def isJinmei: Boolean = bits(Jinmei.ordinal)

    private val bits = bitSet(sources, joyo, jinmei)

  object Sources extends ThrowsDomainException:
    private enum Bits:
      case G, H, J, K, T, V, Joyo, Jinmei
    private object Bits:
      private lazy val sourceValues = values.takeWhile(_ != Joyo)

      def bitSet(sources: String, joyo: Boolean, jinmei: Boolean): BitSet = sources.foldLeft(if joyo
        then if jinmei then error("Sources can't be both joyo and jinmei") else BitSet(Joyo.ordinal)
        else if jinmei then BitSet(Jinmei.ordinal)
        else BitSet())((bits, x) =>
        bits + Try(valueOf(x.toString).ordinal)
          .getOrElse(error(s"Sources got unrecognized region '$x'")))

      def sourceString(bits: BitSet): String = sourceValues.filter(x => bits(x.ordinal))
        .foldLeft("")(_ + _.toString)
