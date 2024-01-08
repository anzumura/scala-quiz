package quiz

import scala.util.Try

object Main:
  def main(args: Array[String]): Unit = Try(Quiz().start()).failed.foreach(e => println("got " +
    "exception: " + e))
