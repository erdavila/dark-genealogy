package dark.display.graph

import dark.display.graph.geometry._

trait Drawable {
  def dimensions: Dimensions
  def drawOver(lines: FansiLines, leftTop: Position)(implicit coloring: Coloring): FansiLines
}

object Drawable {
  private val MinParentLinkLineLength = 1
  private val ChildLinkLineLength = 1

  lazy val nothing: Drawable =
    new Drawable {
      override lazy val dimensions: Dimensions = Dimensions(0, 0)
      override def drawOver(lines: FansiLines, leftTop: Position)(implicit coloring: Coloring): FansiLines = lines
    }

  def blanksLine(width: Int)(implicit coloring: Coloring): fansi.Str =
    coloring.blank(' '.toString * width)

  object OnPath extends PathDependentColoring {
    override protected def nameColoring(name: String)(implicit coloring: Coloring): fansi.Str = coloring.onPathName(name)
    override protected def lineColoring(str: String)(implicit coloring: Coloring): fansi.Str = coloring.onPathLine(str)

    def parentToParentLink(minLength: Int): HorizontalDrawable = {
      val length = math.max(
        minLength,
        leftParentLink.dimensions.width + rightParentLink.dimensions.width + 1,
      )
      val middleLength = length - (leftParentLink.dimensions.width + rightParentLink.dimensions.width)
      leftParentLink ++ horizontalLine(middleLength) ++ rightParentLink
    }

    def childLink(linkToLeft: Boolean, linkToRight: Boolean): Drawable =
      new VerticalDrawable(1 + ChildLinkLineLength) {
        override def content(implicit coloring: Coloring): fansi.Str = {
          val firstChar = (linkToLeft, linkToRight) match {
            case (true, true) => '┬'
            case (true, false) => '┐'
            case (false, true) => '┌'
            case (false, false) => '╷'
          }
          val chars = firstChar +: Seq.fill(ChildLinkLineLength)('│')
          lineColoring(chars.mkString)
        }
      }

    def child(childName: String): Drawable =
      new HorizontalDrawable(childName.length) {
        override def content(implicit coloring: Coloring): fansi.Str = nameColoring(childName)
      }
  }

  object OffPath extends PathDependentColoring {
    override protected def nameColoring(name: String)(implicit coloring: Coloring): fansi.Str = coloring.offPathName(name)
    override protected def lineColoring(str: String)(implicit coloring: Coloring): fansi.Str = coloring.offPathLine(str)
  }

  trait PathDependentColoring {
    protected def nameColoring(name: String)(implicit coloring: Coloring): fansi.Str
    protected def lineColoring(str: String)(implicit coloring: Coloring): fansi.Str

    def name(name: String): HorizontalDrawable =
      new HorizontalDrawable(name.length) {
        override def content(implicit coloring: Coloring): fansi.Str = nameColoring(name)
      }

    def leftParentAndLink(leftParentName: String): HorizontalDrawable =
      name(leftParentName) ++ leftParentLink

    def rightParentAndLink(rightParentName: String): HorizontalDrawable =
      rightParentLink ++ name(rightParentName)

    lazy val leftParentLink: HorizontalDrawable =
      LinkTip.leftParent ++ horizontalLine(MinParentLinkLineLength)

    lazy val rightParentLink: HorizontalDrawable =
      horizontalLine(MinParentLinkLineLength) ++ LinkTip.rightParent

    def horizontalLine(length: Int): HorizontalDrawable =
      new HorizontalDrawable(length) {
        override def content(implicit coloring: Coloring): fansi.Str =
          lineColoring('─'.toString * length)
      }

    private object LinkTip {
      lazy val leftParent: HorizontalDrawable =
        new HorizontalDrawable(1) {
          override def content(implicit coloring: Coloring): fansi.Str = lineColoring("╶")
        }

      lazy val rightParent: HorizontalDrawable =
        new HorizontalDrawable(1) {
          override def content(implicit coloring: Coloring): fansi.Str = lineColoring("╴")
        }
    }
  }

  abstract class HorizontalDrawable(length: Int) extends BaseDrawable { self =>
    def content(implicit coloring: Coloring): fansi.Str
    override lazy val dimensions: Dimensions = Dimensions(height = 1, width = length)
    override def contentLines(implicit coloring: Coloring): FansiLines = Seq(content)

    def ++ (other: HorizontalDrawable): HorizontalDrawable =
      new HorizontalDrawable(self.dimensions.width + other.dimensions.width) {
        override def content(implicit coloring: Coloring): fansi.Str = self.content ++ other.content
      }
  }

  private abstract class VerticalDrawable(length: Int) extends BaseDrawable {
    def content(implicit coloring: Coloring): fansi.Str
    override lazy val dimensions: Dimensions = Dimensions(height = length, width = 1)
    override def contentLines(implicit coloring: Coloring): FansiLines = {
      val c = content
      for (i <- 0 until c.length)
        yield c.substring(i, i + 1)
    }
  }

  abstract class BaseDrawable extends Drawable {
    def contentLines(implicit coloring: Coloring): FansiLines

    override def drawOver(lines: FansiLines, leftTop: Position)(implicit coloring: Coloring): FansiLines = {
      val rectangle = Rectangle(leftTop, dimensions)

      val linesBefore = lines.take(rectangle.top)
      val originalLines = lines.slice(rectangle.top, rectangle.bottom)
      val linesAfter = lines.drop(rectangle.bottom)

      val drawnOverLines =
        for ((line, contentLine) <- originalLines `zip` contentLines)
          yield line.substring(0, rectangle.left) ++ contentLine ++ line.substring(rectangle.right)

      linesBefore ++ drawnOverLines ++ linesAfter
    }
  }

  implicit class DrawableOps[A <: Drawable](private val drawable: A) extends AnyVal {
    def topLeftAt(position: Position): Positioned[A] =
      Positioned(drawable, position)

    def topMiddleAt(position: Position): Positioned[A] =
      topLeftAt(position - Offset.columns(drawable.dimensions.halfWidth))

    def topRightAt(position: Position): Positioned[A] =
      topLeftAt(position - Offset.columns(drawable.dimensions.width))

    def bottomLeftAt(position: Position): Positioned[A] =
      topLeftAt(position - Offset.lines(drawable.dimensions.height))
  }
}
