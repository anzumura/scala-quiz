package quiz

import java.nio.file.{Files, Path}

/**
 * holds data loaded from files with Kanji string entries
 *
 * Kanji can be specified either one per line or multiple per line separated by
 * space. Uniqueness is verified when data is loaded and entries are stored in
 * order in a list. There are derived classes for specific data types, i.e.,
 * where all entries are for a 'JLPT Level' or a 'Kentei Kyu'.
*/
class ListFile {}

object ListFile extends ThrowsDomainException {
  val TextFileExtension = ".txt"

  /**
   * @param dir directory to search
   * @param file file name inside `dir`. Note, if `file` isn't found and the
   *             name given doesn't have an extension then the function will
   *             alternatively look for `file.txt`
   * @return full path of valid regular file
   * @throws DomainException if `dir` isn't a directory
   * @throws DomainException if `file` is an absolute path
   * @throws DomainException if `file` doesn't exist in `dir`
   * @throws DomainException if resulting path is not a regular file
   */
  def getFile(dir: Path, file: Path): Path = {
    if (!Files.isDirectory(dir)) error(s"'$dir' is not a directory")
    if (file.isAbsolute) error(s"'$file' is an absolute path")
    var result = dir.resolve(file)
    if (!Files.exists(result)) {
      val name = result.getFileName.toString
      if (
        name.contains('.') || {
          result = result.getParent.resolve(name + TextFileExtension)
          !Files.exists(result)
        }
      ) error(s"'$result' not found")
    }
    if (!Files.isRegularFile(result)) error(s"'$result' is not a regular file")
    result
  }
}
