package quiz.main

import quiz.data.KanjiData
import quiz.kanji.KanjiType

import scala.util.Try

object Quiz {
  def main(args: Array[String]): Unit = Try {
    val data = KanjiData(KanjiData.dataDir())
    KanjiType.values.foreach { t => println(s"Loaded ${data.getType(t).size} $t Kanji") }
  }.failed.foreach(e => println("got exception: " + e))
}
