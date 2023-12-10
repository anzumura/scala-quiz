package quiz.kanji

import quiz.kanji.UcdData.{LinkType, Source, Ucd, UcdFileName}
import quiz.utils.ColumnFile.AllowExtraCols.Yes
import quiz.utils.ColumnFile.{AllowExtraCols, Column}
import quiz.utils.UnicodeUtils.Code
import quiz.utils.{ColumnFile, DomainException, ThrowsDomainException}

import java.nio.file.Path
import scala.collection.immutable.BitSet
import scala.collection.mutable
import scala.util.{Success, Try}

class UcdData(dir: Path) extends ThrowsDomainException {
  def find(s: String): Option[Ucd] = data.get(s)
  inline def size: Int = data.size

  private lazy val data = {
    val codeCol = Column("Code")
    val radicalCol = Column("Radical")
    val strokesCol = Column("Strokes")
    val pinyinCol = Column("Pinyin")
    val morohashiIdCol = Column("MorohashiId")
    val nelsonIdsCol = Column("NelsonIds")
    val sourcesCol = Column("Sources")
    val jSourceCol = Column("JSource")
    val joyoCol = Column("Joyo")
    val jinmeiCol = Column("Jinmei")
    val linkCodesCol = Column("LinkCodes")
    val linkTypeCol = Column("LinkType")
    val meaningCol = Column("Meaning")
    val japaneseCol = Column("Japanese")
    val f = ColumnFile(
      dir.resolve(UcdFileName),
      AllowExtraCols.Yes,
      codeCol,
      radicalCol,
      strokesCol,
      pinyinCol,
      morohashiIdCol,
      nelsonIdsCol,
      sourcesCol,
      jSourceCol,
      joyoCol,
      jinmeiCol,
      linkCodesCol,
      linkTypeCol,
      meaningCol,
      japaneseCol
    )
    f.processRows(mutable.Buffer[Ucd]())(_ += Ucd(
        Code.fromHex(f.get(codeCol)),
        f.get(radicalCol),
        f.getUInt(strokesCol),
        f.get(pinyinCol),
        f.getOption(morohashiIdCol).map(MorohashiId(_)),
        nelsonIds(f.get(nelsonIdsCol)),
        Source(f.get(jSourceCol), f.get(sourcesCol), f.getBool(joyoCol), f.getBool(jinmeiCol)),
        links(f.get(linkCodesCol)),
        f.getOption(linkTypeCol).map(x => LinkType.valueOf(x.replace("*", "_R")))
          .getOrElse(LinkType.None),
        f.get(meaningCol),
        f.get(japaneseCol)
      )).map(x => x.code.toUTF16 -> x).toMap
  }

  private def nelsonIds(s: String): List[Int] = {
    if (s.isEmpty) Nil
    else Try(s.split(",").map(Integer.parseInt)) match {
      case Success(x) => x.toList
      case _ => throw DomainException(s"invalid NelsonIds '$s'")
    }
  }

  private def links(s: String): List[Code] = {
    if (s.isEmpty) Nil
    else Try(s.split(",").map(Code.fromHex)) match {
      case Success(x) => x.toList
      case _ => throw DomainException(s"invalid LinkCodes '$s'")
    }
  }
}

object UcdData {
  val UcdFileName = "ucd.txt"
  /** represents the XML property from which the link was loaded. '_R' means the link was also
   *  used to pull in readings. '_R' come first in the enum to allow '<' comparison to find all
   *  reading links. Note, there is no non '_R' type for 'Semantic' links by design.
   */
  enum LinkType extends NoneEnum[LinkType](LinkType) {
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
  object LinkType extends NoneEnumObject[LinkType]

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
  class Source private (val jSource: String, sources: String, joyo: Boolean, jinmei: Boolean) {
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
    def apply(jSource: String, source: String, joyo: Boolean, jinmei: Boolean): Source =
      new Source(jSource, source, joyo, jinmei)

    private enum Bits {
      case G, H, J, K, T, V, Joyo, Jinmei
    }
    private object Bits {
      private lazy val sourceValues = values.takeWhile(_ != Joyo)

      def bitSet(sources: String, joyo: Boolean, jinmei: Boolean): BitSet = sources
        .foldLeft(if (joyo) {
          if (jinmei) BitSet(Joyo.ordinal, Jinmei.ordinal) else BitSet(Joyo.ordinal)
        } else if (jinmei) BitSet(Jinmei.ordinal)
          else BitSet())((bits, x) => bits + valueOf(x.toString).ordinal)

      def sourceString(bits: BitSet): String = sourceValues.filter(x => bits(x.ordinal))
        .foldLeft("")(_ + _.toString)
    }
  }

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
  case class Ucd(code: Code, radical: String, strokes: Int, pinyin: String,
      morohashiId: Option[MorohashiId], nelsonIds: List[Int], source: Source, links: List[Code],
      linkType: LinkType, meaning: String, reading: String) {
    def jSource: String = source.jSource
    def joyo: Boolean = source.isJoyo
    def jinmei: Boolean = source.isJinmei
  }
}
