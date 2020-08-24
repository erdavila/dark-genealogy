package dark.display.geometry

trait HasDimensions {
  def dimensions: Dimensions
}

object HasDimensions {
  implicit class HasDimensionsOps[A <: HasDimensions](private val hd: A) extends AnyVal {
    def topLeftAt(position: Position): Positioned[A] =
      Positioned(hd, position)

    def topMiddleAt(position: Position): Positioned[A] =
      topLeftAt(position - Offset.columns(hd.dimensions.halfWidth))

    def topRightAt(position: Position): Positioned[A] =
      topLeftAt(position - Offset.columns(hd.dimensions.width))

    def bottomLeftAt(position: Position): Positioned[A] =
      topLeftAt(position - Offset.lines(hd.dimensions.height))

    def bottomMiddleAt(position: Position): Positioned[A] =
      bottomLeftAt(position - Offset.columns(hd.dimensions.halfWidth))
  }
}
