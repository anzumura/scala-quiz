package quiz

class DomainException(msg: String) extends Exception(msg) {}

object DomainException {
  def apply(msg: String): DomainException = new DomainException(msg)
}
