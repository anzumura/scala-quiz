package quiz.kanji

trait OrderedEnum[T <: OrderedEnum[T]] extends Ordered[T] {
  def ordinal: Int
  override def compare(that: T): Int = ordinal - that.ordinal
}

/** base class for `enum` with a "None" value (which means it ends up becoming the base class of
 *  each enum value)
 *  @param obj the `enum` object instance, i.e., "Grade", "Level", etc.
 */
trait NoneEnum[T <: NoneEnum[T]](obj: NoneEnumObject[T]) extends OrderedEnum[T] {
  /** returns true if this enum value is not the "None" value */
  lazy val isDefined: Boolean = toString != "None" // maybe there's a nicer way
  /** the name of the `enum` class, i.e., "Grade", "Level", etc. */
  val enumName: String = obj.enumName
}

/** base class for companion object of `enum` with a "None" value */
trait NoneEnumObject[T <: NoneEnum[T]] {
  /** array of all values except the "None" value */
  lazy val defined: Array[T] = values.filter(_.isDefined)
  /** the name of the `enum` class, i.e., "Grade", "Level", etc. */
  val enumName: String = getClass.getSimpleName.dropRight(1)

  /** `values` is overridden by compiler generated `enum` object */
  def values: Array[T]

  /** returns true if `v` is not the "None" value */
  def isDefined(v: T): Boolean = v.isDefined
}
