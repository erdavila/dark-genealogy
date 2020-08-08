package dark.display.graph.geometry

case class Position(line: Int, column: Int) {
  def +(offset: Offset): Position = Position(
    line = line + offset.lines,
    column = column + offset.columns,
  )

  def -(offset: Offset): Position =
    this + -offset

  def asOffset: Offset =
    Offset(lines = line, columns = column)
}
