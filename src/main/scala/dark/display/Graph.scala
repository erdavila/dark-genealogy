package dark.display

import dark.Path.Path
import dark.display.graph.Coloring

object Graph {
  def get(path: Path): Seq[String] = {
    implicit val coloring: Coloring = new Coloring {
      private val sourceName = path.head.source.name
      private val targetName = path.last.target.name
      private val color = fansi.Color.Green

      override def onPathName(name: String): fansi.Str = {
        val maybeBold = if (name == sourceName || name == targetName) fansi.Bold.On else fansi.Bold.Off
        (color ++ maybeBold)(name)
      }
      override def onPathLine(line: String): fansi.Str = color(line)
      override def offPathName(name: String): fansi.Str = name
      override def offPathLine(line: String): fansi.Str = line
      override def blank(str: String): fansi.Str = str
    }

    graph.Graph.from(path)
      .draw
      .map(_.render)
  }
}
