package quiz.kanji

trait BaseKanjiData:
  /** JLPT level of `s` or "NoLevel" if it doesn't have a level */
  def level(s: String): Level
  /** Kentei kyu of `s` or "NoKyu" if it doesn't have a kyu */
  def kyu(s: String): Kyu
  /** frequency of `s` starting at 1 or 0 if it doesn't have a frequency */
  def frequency(s: String): Int

  def getUcd(s: String): Ucd
  def getRadical(s: String): Radical

  def params(name: String): Kanji.Params = Kanji.Params(this, name)
  def params(name: String, ucd: Ucd): Kanji.Params = Kanji.Params(this, name, ucd)
