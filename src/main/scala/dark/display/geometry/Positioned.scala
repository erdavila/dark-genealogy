package dark.display.geometry

case class Positioned[+A <: HasDimensions](thing: A, topLeftPosition: Position) {
  lazy val rectangle: Rectangle = Rectangle(topLeftPosition, thing.dimensions)

  def offset(offset: Offset): Positioned[A] =
    Positioned(thing, topLeftPosition + offset)
}
