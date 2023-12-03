package quiz

import java.nio.file.Path
import scala.reflect.ClassTag

class KanjiData protected (val path: Path) {
  private lazy val levels = load(Level)
  private lazy val kyus = load(Kyu)

  /** return JLPT level for given `s` or "None" if it doesn't have a level */
  def level(s: String): Level.Value = levels.getOrElse(s, Level.None)

  /** return Kentei kyu for given `s` or "None" if it doesn't have a kyu */
  def kyu(s: String): Kyu.Value = kyus.getOrElse(s, Kyu.None)

  private def load[T <: EnumWithNone: ClassTag](e: T): Map[String, T#Value] = {
    e.defined.map(EnumListFile(path.resolve(e.toString), _)).flatMap(f =>
      f.entries.map(_ -> f.value)
    ).toMap
  }
}

object KanjiData {
  def apply(path: Path): KanjiData = new KanjiData(path)
}
