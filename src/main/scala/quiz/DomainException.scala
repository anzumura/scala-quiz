package quiz

class DomainException(msg: String) extends Exception(msg) {}

object DomainException {
  def apply(msg: String, fromClass: Option[String] = None): DomainException =
    new DomainException(fromClass.map(c => s"[$c] $msg").getOrElse(msg))
}

trait ThrowsDomainException {
  /** throws a DomainException that includes the class name at the beginning */
  protected def domainError(msg: String): Nothing = {
    val c = getClass.getSimpleName
    throw DomainException(msg,
      Option(if (c.endsWith("$")) c.dropRight(1) else c))
  }

  /** throws a DomainException that just contains `msg` */
  protected def error(msg: String): Nothing = throw DomainException(msg)
}
