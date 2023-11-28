package quiz

import java.nio.file.{Files, Path}
import scala.annotation.tailrec

object FileUtils extends ThrowsDomainException {
  val TextFileExtension = ".txt"

  def fileName(path: Path): String = path.getFileName.toString

  /**
   * @param path path to check for an extension
   * @return true if `path` has an extension
   */
  def hasExtension(path: Path): Boolean = fileName(path).contains('.')

  def addExtension(path: Path, extension: String): Path =
    (Option(path.getParent), fileName(path) + extension) match {
      case (Some(parent), file) => parent.resolve(file)
      case (_, file) => Path.of(file)
    }

  /**
   * @param path path to check for existence
   * @param extension if `path` doesn't exist and also doesn't have an extension
   *                  then optionally check existence of "path + extension"
   * @return path to existing file
   * @throws DomainException if `path` doesn't exist
   */
  @tailrec
  def checkExists(path: Path, extension: Option[String] = None): Path = {
    if (Files.exists(path)) path
    else extension match {
      case Some(ext) if !hasExtension(path) => checkExists(addExtension(path, ext))
      case _ => error(s"'$path' not found")
    }
  }

  /**
   * @param dir  directory to search
   * @param file file name inside `dir`.
   * @param extension optionally add extension to file if it doesn't exist
   * @return full path of valid regular file
   * @throws DomainException if `dir` isn't a directory
   * @throws DomainException if `file` is an absolute path
   * @throws DomainException if `file` doesn't exist in `dir`
   * @throws DomainException if resulting path is not a regular file
   */
  def resolve(dir: Path, file: Path, extension: Option[String] = None): Path = {
    if (!Files.isDirectory(dir)) error(s"'$dir' is not a directory")
    if (file.isAbsolute) error(s"'$file' is an absolute path")
    val result = checkExists(dir.resolve(file), extension)
    if (!Files.isRegularFile(result)) error(s"'$result' is not a regular file")
    result
  }

  def textFile(dir: Path, file: Path): Path =
    resolve(dir, file, Option(TextFileExtension))
}