package dark.display.snake

import dark.Path.Path
import dark.Relation.{ChildOf, ParentOf}
import dark.display.geometry._
import scala.annotation.tailrec

object Snake {

  def from(path: Path): Seq[String] = {
    val positionedStringsByLine = positionedSegments(Vector.empty, path)
      .flatMap(_.positionedStrings)
      .groupBy(_.rectangle.top)
      .view.mapValues(pss => pss.sortBy(_.rectangle.left))
      .toSeq
      .sortBy(_._1)

    if (true) {
      for (positionedStrings <- positionedStringsByLine.map(_._2))
        yield
          positionedStrings.foldLeft("") { (line, positionedString) =>
            assert(positionedString.rectangle.left >= line.length)
            line ++ (" " * (positionedString.rectangle.left - line.length)) ++ positionedString.thing.string
          }
    } else { // DEBUG
      for ((i, line) <- positionedStringsByLine)
        yield s"$i:: " ++ line.map(x => s"${x.rectangle.left}:${x.thing.string}").mkString("  ")
    }
  }

  private sealed trait Segment extends HasDimensions {
    def stringsWithDimensions: Vector[StringWithDimensions]
  }

  private sealed trait VerticalNames extends Segment {
    val names: Vector[String]

    override lazy val dimensions: Dimensions = Dimensions(
      height = 2 * names.length - 1,
      width = names.map(_.length).max,
    )

    protected def namesTopDown: Vector[String]

    override def stringsWithDimensions: Vector[StringWithDimensions] = {
      val namesWithDimensions = for (name <- namesTopDown) yield Name(name)
      namesWithDimensions.head +: namesWithDimensions.tail.flatMap { nwd =>
        Seq(VerticalLine, nwd)
      }
    }
  }

  private case class VerticalDownNames(names: Vector[String]) extends VerticalNames {
    override protected def namesTopDown: Vector[String] = names
  }

  private object VerticalDownNames {
    object fromPath {
      def unapply(path: Path): Option[(VerticalDownNames, Path)] = {
        @tailrec
        def loop(path: Path, names: Vector[String]): (Vector[String], Path) =
          path match {
            case ParentOf(_, _, _) +: ChildOf(_, _, _) +: _ => (names, path)
            case ParentOf(_, child, _) +: restPath => loop(restPath, names :+ child.name)
            case _ => (names, path)
          }

        val (names, restPath) = loop(path, Vector.empty)
        Option.when(names.nonEmpty) {
          (VerticalDownNames(path.head.source.name +: names), restPath)
        }
      }
    }
  }

  private case class VerticalUpNames(names: Vector[String]) extends VerticalNames {
    override protected def namesTopDown: Vector[String] = names.reverse
  }

  private object VerticalUpNames {
    object fromPath {
      def unapply(path: Path): Option[(VerticalUpNames, Path)] = {
        @tailrec
        def loop(path: Path, names: Vector[String]): (Vector[String], Path) =
          path match {
            case ChildOf(_, _, _) +: ParentOf(_, _, _) +: _ => (names, path)
            case ChildOf(_, parent, _) +: restPath => loop(restPath, names :+ parent.name)
            case _ => (names, path)
          }

        val (names, restPath) = loop(path, Vector.empty)
        Option.when(names.nonEmpty) {
          (VerticalUpNames(path.head.source.name +: names), restPath)
        }
      }
    }
  }

  private sealed abstract class StringWithDimensions(val string: String) extends Segment {
    override lazy val dimensions: Dimensions = Dimensions(height = 1, width = string.length)
    override lazy val stringsWithDimensions: Vector[this.type] = Vector(this)
  }

  private case class Name(name: String) extends StringWithDimensions(name)
  private case object VerticalLine extends StringWithDimensions("|")
  private case object DiagonalDown extends StringWithDimensions("\\")
  private case object DiagonalUp extends StringWithDimensions("/")

  private case class ^^^[A, B](a: A, b: B) {
    override def toString: String = a.toString ++ " ^^^ " ++ b.toString
  }
  private implicit class AnyOps[A](private val a: A) extends AnyVal {
    def ^^^[B](b: B): A ^^^ B = Snake.^^^(a, b)
  }

  private val HorizontalSpacing = 3

  @tailrec
  private def positionedSegments(posSegs: Vector[Positioned[Segment]], path: Path): Vector[Positioned[Segment]] =
    posSegs ^^^ path match {
      case pSs :+ posL :+ (posDU@Positioned(DiagonalUp, _)) :+ (posP@Positioned(Name(_), _)) ^^^ ParentOf(_, child, _) +: restPath =>
        assert(posL match { case Positioned(Name(_), _) => true; case _ => false }) // TODO: remove!
        val (r, restPath2) = restPath match {
          case VerticalDownNames.fromPath(vdn, rp) => (vdn, rp)
          case _ => (Name(child.name), restPath)
        }
        val posR = r.topLeftAt(posL.rectangle.topRight + Offset.columns(HorizontalSpacing))
        val (reposDU, reposP) = {
          val reposP = posP.thing.topMiddleAt(topMiddleBetween(posL, posR.positionedStrings.head) - Offset.lines(2))
          if (reposP.topLeftPosition.column > posP.topLeftPosition.column) {
            val reposDU = DiagonalUp.topMiddleAt(topMiddleBetween(posL, reposP))
            (reposDU, reposP)
          } else {
            (posDU, posP)
          }
        }
        val positionedDiagDown = DiagonalDown.topMiddleAt(topMiddleBetween(reposP, posR.positionedStrings.head))
        positionedSegments(
          pSs :+ posL :+ reposDU :+ reposP :+ positionedDiagDown :+ posR,
          restPath2,
        )
      case pSs :+ posL :+ (posDD@Positioned(DiagonalDown, _)) :+ (posC@Positioned(Name(_), _)) ^^^ ChildOf(_, parent, _) +: restPath =>
        assert(posL match { case Positioned(Name(_), _) => true; case _ => false }) // TODO: remove!
        val (r, restPath2) = restPath match {
          case VerticalUpNames.fromPath(vun, rp) => (vun, rp)
          case _ => (Name(parent.name), restPath)
        }
        val posR = r.bottomLeftAt(posL.rectangle.bottomRight + Offset.columns(HorizontalSpacing))
        val (reposDD, reposC) = {
          val reposC = posC.thing.topMiddleAt(topMiddleBetween(posL, posR.positionedStrings.last) + Offset.lines(2))
          if (reposC.topLeftPosition.column > posC.topLeftPosition.column) {
            val reposDD = DiagonalDown.topMiddleAt(topMiddleBetween(posL, reposC))
            (reposDD, reposC)
          } else {
            (posDD, posC)
          }
        }
        val positionedDiagUp = DiagonalUp.topMiddleAt(topMiddleBetween(reposC, posR.positionedStrings.last))
        positionedSegments(
          pSs :+ posL :+ reposDD :+ reposC :+ positionedDiagUp :+ posR,
          restPath2,
        )
      case Vector() ^^^ VerticalDownNames.fromPath(vdn, restPath) =>
        positionedSegments(
          Vector(vdn.topLeftAt(Position(0, 0))),
          restPath,
        )
      case Vector() ^^^ VerticalUpNames.fromPath(vun, restPath) =>
        positionedSegments(
          Vector(vun.bottomLeftAt(Position(0, 0))),
          restPath,
        )
      case _ ^^^ ParentOf(parentL, _, _) +: ChildOf(child, parentR, _) +: restPath =>
        val (pSs, posL) = posSegs match {
          case _ :+ (pL@Positioned(VerticalDownNames(_) | Name(_), _)) => (posSegs, pL)
          case Vector() =>
            val pL = Name(parentL.name).topLeftAt(Position(0, 0))
            (Vector(pL), pL)
        }
        val (r, restPath2) = restPath match {
          case VerticalUpNames.fromPath(vun, rp) => (vun, rp)
          case _ => (Name(parentR.name), restPath)
        }
        val posR = r.bottomLeftAt(posL.rectangle.bottomRight + Offset.columns(HorizontalSpacing))
        val positionedChild = Name(child.name).topMiddleAt(
          topMiddleBetween(posL.positionedStrings.last, posR.positionedStrings.last) + Offset.lines(2)
        )
        val positionedDiagDown = DiagonalDown.topMiddleAt(topMiddleBetween(posL.positionedStrings.last, positionedChild))
        val positionedDiagUp = DiagonalUp.topMiddleAt(topMiddleBetween(positionedChild, posR.positionedStrings.last))
        positionedSegments(
          pSs :+ positionedDiagDown :+ positionedChild :+ positionedDiagUp :+ posR,
          restPath2,
        )
      case _ ^^^ ChildOf(childL, _, _) +: ParentOf(parent, child, _) +: restPath =>
        val (pSs, posL) = posSegs match {
          case _ :+ (pL@Positioned(VerticalUpNames(_) | Name(_), _)) => (posSegs, pL)
          case Vector() =>
            val pL = Name(childL.name).bottomLeftAt(Position(0, 0))
            (Vector(pL), pL)
        }
        val (r, restPath2) = restPath match {
          case VerticalDownNames.fromPath(vdn, rp) => (vdn, rp)
          case _ => (Name(child.name), restPath)
        }
        val posR = r.topLeftAt(posL.rectangle.topRight + Offset.columns(HorizontalSpacing))
        val positionedParent = Name(parent.name).topMiddleAt(
          topMiddleBetween(posL.positionedStrings.head, posR.positionedStrings.head) - Offset.lines(2)
        )
        val positionedDiagUp = DiagonalUp.topMiddleAt(topMiddleBetween(posL.positionedStrings.head, positionedParent))
        val positionedDiagDown = DiagonalDown.topMiddleAt(topMiddleBetween(positionedParent, posR.positionedStrings.head))
        positionedSegments(
          pSs :+ positionedDiagUp :+ positionedParent :+ positionedDiagDown :+ posR,
          restPath2,
        )
      case posSegs ^^^ Vector() => posSegs
    }

  private def topMiddleBetween(p1: Positioned[_], p2: Positioned[_]) =
    Position(
      line = (p1.rectangle.top + p2.rectangle.top) / 2,
      column = (p1.rectangle.right + p2.rectangle.left) / 2,
    )

  private implicit class PositionedSegmentOps(private val ps: Positioned[Segment]) extends AnyVal {
    def positionedStrings: Seq[Positioned[StringWithDimensions]] = {
      for ((str, i) <- ps.thing.stringsWithDimensions.zipWithIndex)
        yield Positioned(
          str,
          ps.topLeftPosition + Offset(lines = i, columns = (ps.thing.dimensions.width - str.dimensions.width) / 2)
        )
    }
  }
}
