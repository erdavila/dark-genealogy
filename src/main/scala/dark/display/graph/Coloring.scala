package dark.display.graph

trait Coloring {
  def onPathName(name: String): fansi.Str
  def onPathLine(line: String): fansi.Str
  def offPathName(name: String): fansi.Str
  def offPathLine(line: String): fansi.Str
  def blank(str: String): fansi.Str
}
