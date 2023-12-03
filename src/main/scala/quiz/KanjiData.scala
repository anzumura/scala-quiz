package quiz

import quiz.FileUtils.textFile

import java.nio.file.Path
import scala.reflect.ClassTag

class KanjiData protected (val path: Path) {
  private lazy val levels = load(Level)
  private lazy val kyus = load(Kyu)
  private lazy val frequencies =
    KanjiListFile(textFile(path, "frequency")).indices.map { case (k, v) =>
      (k, v + 1)
    }

  /** JLPT level of `s` or "None" if it doesn't have a level */
  def level(s: String): Level.Value = levels.getOrElse(s, Level.None)

  /** Kentei kyu of `s` or "None" if it doesn't have a kyu */
  def kyu(s: String): Kyu.Value = kyus.getOrElse(s, Kyu.None)

  /** frequency of `s` starting at 1 or 0 if it doesn't have a frequency */
  def frequency(s: String): Int = frequencies.getOrElse(s, 0)

  private def load[T <: EnumWithNone: ClassTag](e: T): Map[String, T#Value] = {
    e.defined.map(EnumListFile(path.resolve(e.toString), _)).flatMap(f =>
      f.entries.map(_ -> f.value)
    ).toMap
  }
}

object KanjiData {
  def apply(path: Path): KanjiData = new KanjiData(path)
}
