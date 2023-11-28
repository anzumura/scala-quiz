package quiz

import java.nio.file.{Files, Path}
import scala.annotation.tailrec

/**
 * holds data loaded from files with Kanji string entries
 *
 * Kanji can be specified either one per line or multiple per line separated by
 * space. Uniqueness is verified when data is loaded and entries are stored in
 * order in a list. There are derived classes for specific data types, i.e.,
 * where all entries are for a 'JLPT Level' or a 'Kentei Kyu'.
 */
class ListFile {}

object ListFile extends ThrowsDomainException {}
