package dark.display.graph.geometry

import dark.display.graph.{Coloring, Drawable, FansiLines}

case class Positioned[+A <: Drawable](drawable: A, topLeftPosition: Position) {
  lazy val rectangle: Rectangle = Rectangle(topLeftPosition, drawable.dimensions)

  def drawOver(lines: FansiLines)(implicit coloring: Coloring): FansiLines =
    drawable.drawOver(lines, topLeftPosition)

  def offset(offset: Offset): Positioned[A] =
    Positioned(drawable, topLeftPosition + offset)
}
