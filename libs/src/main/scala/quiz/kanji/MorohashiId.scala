package quiz.kanji

import quiz.kanji.MorohashiId.IndexType
import quiz.kanji.MorohashiId.IndexType.*
import quiz.utils.ThrowsDomainException

import scala.util.{Success, Try}

final case class MorohashiId private (index: Int, indexType: IndexType):
  override def toString: String =
    (indexType match
      case Plain => "%05d"
      case Prime => "%05dP"
      case DoublePrime => "%05dPP"
      case Supplemental => "H%03d"
    ).formatted(index)

object MorohashiId extends ThrowsDomainException:
  val MaxIndex: Int = 99999
  val MaxSupplementalIndex: Int = 999

  def apply(id: String): MorohashiId =
    if id.isEmpty then domainError("empty index")
    val (index, indexType) =
      if id.startsWith("H") then (id.drop(1), Supplemental)
      else if id.endsWith("PP") then (id.dropRight(2), DoublePrime)
      else if id.endsWith("P") then (id.dropRight(1), Prime)
      else (id, Plain)
    Try(index.toInt) match
      case Success(x) => apply(x, indexType)
      case _ => domainError(s"invalid format '$id'")

  def apply(index: Int, indexType: IndexType = Plain): MorohashiId =
    if index < 0 then domainError("negative index")
    (if indexType == Supplemental then MaxSupplementalIndex else MaxIndex) match
      case x if index > x => domainError(s"$indexType index $index exceeds $x")
      case _ => new MorohashiId(index, indexType)

  enum IndexType:
    case Plain, Prime, DoublePrime, Supplemental
