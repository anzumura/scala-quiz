package quiz.utils

trait OrderedEnum[T <: OrderedEnum[T]] extends Ordered[T] {
  def ordinal: Int
  override def compare(that: T): Int = ordinal - that.ordinal
}

/** base class for `enum` with a "NoXxx" value, i.e., "NoGrade", "NoLevel", etc.
 *  @param obj the `enum` object instance, i.e., "Grade", "Level", etc.
 */
trait NoValueEnum[T <: NoValueEnum[T]](obj: NoValueEnumObject[T]) extends OrderedEnum[T] {
  /** the name of the `enum` class, i.e., "Grade", "Level", etc. */
  val enumName: String = obj.enumName
  /** returns true if this enum value is not the "NoXxx" value */
  lazy val isDefined: Boolean = toString != "No" + enumName
}

/** base class for companion object of `enum` with a "NoXxx" value */
trait NoValueEnumObject[T <: NoValueEnum[T]] {
  /** array of all values except the "NoXxx" value */
  lazy val defined: Array[T] = values.filter(_.isDefined)
  /** the name of the `enum` class, i.e., "Grade", "Level", etc. */
  val enumName: String = getClass.getSimpleName.dropRight(1)

  /** `values` is overridden by compiler generated `enum` object */
  def values: Array[T]

  /** returns true if `v` is not the "NoXxx" value */
  def isDefined(v: T): Boolean = v.isDefined
}
