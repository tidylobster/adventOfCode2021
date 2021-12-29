package main

import scala.io.Source

trait Utils {
  def getData(path: String): String = {
    val source = Source.fromFile(path)
    val input = source.mkString
    source.close()
    input
  }
}
