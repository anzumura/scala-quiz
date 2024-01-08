package quiz.data

import quiz.data.UcdData.*
import quiz.kanji.Ucd.{LinkType, Sources}
import quiz.kanji.{MorohashiId, Ucd}
import quiz.utils.ColumnFile.AllowExtraCols.Yes
import quiz.utils.ColumnFile.{AllowExtraCols, Column}
import quiz.utils.{Code, ColumnFile, DomainException, ThrowsDomainException}

import java.nio.file.Path
import scala.collection.mutable
import scala.util.{Success, Try}

class UcdData(dir: Path, radicalData: RadicalData) extends ThrowsDomainException:
  def find(s: String): Option[Ucd] = data.get(s)
  inline def size: Int = data.size

  lazy val data: Map[String, Ucd] =
    val f = ColumnFile(
      dir.resolve(UcdFileName), AllowExtraCols.Yes, codeCol, radicalCol, strokesCol, pinyinCol,
      morohashiIdCol, nelsonIdsCol, sourcesCol, jSourceCol, joyoCol, jinmeiCol, linkCodesCol,
      linkTypeCol, meaningCol, japaneseCol)
    f.processRows(mutable.Buffer[Ucd]())(_ += Ucd(
        Code.fromHex(f.get(codeCol)), radicalData.findByNumber(f.getUInt(radicalCol)),
        f.getUInt(strokesCol), f.get(pinyinCol), f.getOption(morohashiIdCol).map(MorohashiId(_)),
        nelsonIds(f.get(nelsonIdsCol)),
        Sources(f.get(jSourceCol), f.get(sourcesCol), f.getBool(joyoCol), f.getBool(jinmeiCol)),
        links(f.get(linkCodesCol)),
        f.getOption(linkTypeCol).map(x => LinkType.valueOf(x.replace("*", "_R")))
          .getOrElse(LinkType.NoLinkType), f.get(meaningCol), f.get(japaneseCol)))
      .map(x => x.code.toUTF16 -> x).toMap

  private def nelsonIds(s: String): List[Int] =
    if s.isEmpty then Nil
    else
      Try(s.split(",").map(Integer.parseInt)) match
        case Success(x) if x.forall(v => v > 0 && v <= MaxNelsonId) => x.toList
        case _ => throw DomainException(s"invalid NelsonIds '$s'")

  private def links(s: String): List[Code] =
    if s.isEmpty then Nil
    else
      Try(s.split(",").map(Code.fromHex)) match
        case Success(x) => x.toList
        case _ => throw DomainException(s"invalid LinkCodes '$s'")

object UcdData:
  val UcdFileName = "ucd.txt"
  val MaxNelsonId = 5446

  private val codeCol = Column("Code")
  private val radicalCol = Column("Radical")
  private val strokesCol = Column("Strokes")
  private val pinyinCol = Column("Pinyin")
  private val morohashiIdCol = Column("MorohashiId")
  private val nelsonIdsCol = Column("NelsonIds")
  private val sourcesCol = Column("Sources")
  private val jSourceCol = Column("JSource")
  private val joyoCol = Column("Joyo")
  private val jinmeiCol = Column("Jinmei")
  private val linkCodesCol = Column("LinkCodes")
  private val linkTypeCol = Column("LinkType")
  private val meaningCol = Column("Meaning")
  private val japaneseCol = Column("Japanese")
