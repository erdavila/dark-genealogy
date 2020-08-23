package dark.display.geometry

case class Offset(lines: Int, columns: Int) {
  def unary_- : Offset = Offset(-lines, -columns)
}

object Offset {
  def lines(lines: Int): Offset = Offset(lines, 0)
  def columns(columns: Int): Offset = Offset(0, columns)
}
