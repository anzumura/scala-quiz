package quiz.kanji

import quiz.kanji.Radical.MaxRadical
import quiz.utils.Block.isOneKanji
import quiz.utils.ThrowsDomainException

final class Radical(val number: Int, val name: String, val altNames: List[String],
    val longName: String, val reading: String)
extends ThrowsDomainException {
  if (number < 1 || number > MaxRadical)
    error(s"Radical number $number not between 1 and $MaxRadical")
  if (!isOneKanji(name)) error(s"Radical name '$name' is not a single Kanji")
  altNames.foreach(x => if (!isOneKanji(x)) error(s"Radical altName '$x' is not a single Kanji"))

  override def toString: String = s"$name($number)"
  override def equals(obj: Any): Boolean = obj match {
    case that: Radical => number == that.number
    case _ => false
  }

  override def hashCode(): Int = number.##
}

object Radical {
  val MaxRadical = 214
}
