package quiz

import java.nio.file.Path

class KanjiData protected (path: Path) {
  protected def loadLevels(path: Path): Map[String, Level.Value] = {
    Level.defined.map(EnumListFile(path, _)).flatMap(f =>
      f.entries.map(_ -> f.value)
    ).toMap
  }
}

object KanjiData {
  def apply(path: Path): KanjiData = new KanjiData(path)
}
