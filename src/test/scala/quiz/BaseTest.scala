package quiz

import org.scalatest.{BeforeAndAfterAll, BeforeAndAfterEach}
import org.scalatest.freespec.AnyFreeSpec

import java.nio.file.{Files, Path}
import scala.jdk.StreamConverters.StreamHasToScala

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

trait FileTest extends BaseTest with BeforeAndAfterEach with BeforeAndAfterAll {
  val testFile: String = "test.txt"
  // on Windows tempDir is created in ~/AppData/Local/Temp
  val tempDir: Path = Files.createTempDirectory("tempDir")

  def clearDirectory(d: Path): Unit = {
    Files.walk(d).toScala(LazyList).foreach { f =>
      if (Files.isRegularFile(f)) Files.delete(f)
    }
  }

  // delete all files from 'tempDir' after each test
  override protected def afterEach(): Unit = {
    clearDirectory(tempDir)
  }

  // delete 'tempDir' after all tests
  override protected def afterAll(): Unit = {
    Files.deleteIfExists(tempDir)
  }
}
