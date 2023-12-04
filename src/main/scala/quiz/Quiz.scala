package quiz

import scala.util.Try

object Quiz extends App {
  Try {
    val data = KanjiData(KanjiData.dataDir())
    KanjiType.values.foreach { t =>
      println(s"Loaded ${data.getType(t).size} $t Kanji")
    }
  }.failed.foreach(e => println("got exception: " + e))
}
