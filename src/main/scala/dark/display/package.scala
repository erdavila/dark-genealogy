package dark

import dark.Path.Path

package object display {
  def show(title: String)(pathOption: => Option[Path]): Unit = {
    println(title)
    pathOption match {
      case Some(path) =>
        println(s"  ${OneLine.get(path)}")
        println()
        Sentences.get(path).foreach(sentence => println(s"  $sentence"))
        println()
        Graph.get(path).foreach(line => println(s"  $line"))
        println()
        Snake.get(path).foreach(line => println(s"  $line"))
      case None => println("  Path not found")
    }
  }
}
