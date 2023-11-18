package quiz

case class DomainException(private val msg: String) extends Exception(msg) {}