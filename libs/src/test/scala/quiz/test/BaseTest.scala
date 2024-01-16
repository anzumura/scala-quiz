package quiz.test

import org.scalatest.{Assertion, BeforeAndAfterEach}
import org.scalatest.freespec.AnyFreeSpec
import quiz.kanji.Radical
import quiz.kanji.Ucd.Sources
import quiz.utils.{DomainException, EnumListFile}

trait BaseTest extends AnyFreeSpec with BeforeAndAfterEach:
  /** attempts to return main class name by removing "Test" from this class */
  def mainClassName: String = getClass.getSimpleName.replaceAll("Test", "")

  override protected def beforeEach(): Unit =
    EnumListFile.clearEntryData()
    super.beforeEach()

  override protected def afterEach(): Unit =
    EnumListFile.clearEntryData()
    super.afterEach()

  /** assert that `f` throws a DomainException with given `msg` */
  protected def error(f: => Any, msg: String): Assertion =
    val e = intercept[DomainException] { f }
    assert(e.getMessage == s"$msg")

  /** assert that `f` throws a DomainException and test the message using `t` */
  protected def error(f: => Any, t: String => Boolean): Assertion =
    val e = intercept[DomainException] { f }
    assert(t(e.getMessage), " --- for message: " + e.getMessage)

  /** calls [[error]] with "[mainClassName]" prepended to `msg` */
  protected def domainError(f: => Any, msg: String): Assertion = error(f, s"[$mainClassName] $msg")

  protected def countString(src: String, s: String, count: Int): Assertion = assert(
    src.sliding(s.length).count(_ == s) == count)

object BaseTest:
  val testRadical: Radical = Radical(1, "一", Nil, "", "")
  val emptySources: Sources = Sources("", "", false, false)
