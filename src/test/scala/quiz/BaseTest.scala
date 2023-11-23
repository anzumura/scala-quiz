package quiz

import org.scalatest.freespec.AnyFreeSpec

trait BaseTest extends AnyFreeSpec {
  protected def domainException(f: => Any, msg: String): Unit = {
    val e = intercept[DomainException] { f }
    assert(e.getMessage == msg)
  }

  protected def domainException(f: => Any, t: String => Boolean): Unit = {
    val e = intercept[DomainException] { f }
    assert(t(e.getMessage))
  }
}
