package quiz

case class DomainException(msg: String) extends Exception(msg) {}