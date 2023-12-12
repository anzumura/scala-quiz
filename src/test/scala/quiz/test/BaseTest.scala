package quiz.test

import org.scalatest.freespec.AnyFreeSpec
import quiz.kanji.Radical
import quiz.kanji.Ucd.Sources
import quiz.utils.DomainException

trait BaseTest extends AnyFreeSpec {
  /** attempts to return main class name by removing "Test" from this class */
  def mainClassName: String = getClass.getSimpleName.replaceAll("Test", "")

  /** assert that `f` throws a DomainException with given `msg` */
  protected def error(f: => Any, msg: String): Unit = {
    val e = intercept[DomainException] { f }
    assert(e.getMessage == s"$msg")
  }

  /** assert that `f` throws a DomainException and test the message using `t` */
  protected def error(f: => Any, t: String => Boolean): Unit = {
    val e = intercept[DomainException] { f }
    assert(t(e.getMessage), " --- for message: " + e.getMessage)
  }

  /** calls [[error]] with "[mainClassName]" prepended to `msg` */
  protected def domainError(f: => Any, msg: String): Unit = { error(f, s"[$mainClassName] $msg") }
}

object BaseTest {
  val testRadical: Radical = Radical(1, "一", Nil, "", "")
  val emptySources: Sources = Sources("", "", false, false)
}
