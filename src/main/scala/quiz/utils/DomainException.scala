package quiz.utils

class DomainException(msg: String, fromClass: Option[String] = None)
extends Exception(fromClass.map(c => s"[$c] $msg").getOrElse(msg)) {}

trait ThrowsDomainException {
  /** throws a DomainException that includes the class name at the beginning */
  protected def domainError(msg: String): Nothing = {
    val c = getClass.getSimpleName
    throw DomainException(msg, Option(if (c.endsWith("$")) c.dropRight(1) else c))
  }

  /** throws a DomainException that just contains `msg` */
  protected def error(msg: String): Nothing = throw DomainException(msg)
}
