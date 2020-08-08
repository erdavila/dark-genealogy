package dark.display.graph.geometry

case class Rectangle(top: Int, bottom: Int, left: Int, right: Int) {
  def topLeft: Position = Position(top, left)
  def topRight: Position = Position(top, right)
  def bottomLeft: Position = Position(bottom, left)
  def bottomRight: Position = Position(bottom, right)
  def topMiddle: Position = Position(top, middleColumn)
  def middleColumn: Int = (left + right) / 2
}

object Rectangle {
  def apply(topLeftPosition: Position, dimensions: Dimensions): Rectangle = Rectangle(
    top = topLeftPosition.line,
    bottom = topLeftPosition.line + dimensions.height,
    left = topLeftPosition.column,
    right = topLeftPosition.column + dimensions.width,
  )
}
