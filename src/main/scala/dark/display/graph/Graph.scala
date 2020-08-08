package dark.display.graph

import dark.Path.Path
import dark.Relation.{ChildOf, ParentOf}
import dark.display.graph.geometry.{Dimensions, Offset, Position, Positioned}
import scala.annotation.tailrec

class Graph(val sourceLabel: Positioned[Drawable], val targetLabel: Positioned[Drawable])(allDrawables: Positioned[Drawable]*)
  extends Drawable
{
  override lazy val dimensions: Dimensions =
    Dimensions(
      height = allDrawables.view.map(_.rectangle.bottom).max,
      width = allDrawables.view.map(_.rectangle.right).max,
    )

  def draw(implicit coloring: Coloring): FansiLines = {
    val line = Drawable.blanksLine(dimensions.width)
    val lines = Seq.fill(dimensions.height)(line)
    allDrawables.foldLeft(lines) { case (lines, d) =>
      d.drawOver(lines)
    }
  }

  override def drawOver(lines: FansiLines, leftTop: Position)(implicit coloring: Coloring): FansiLines = {
    val offset = leftTop.asOffset
    allDrawables.foldLeft(lines) { case (lines, d) =>
      d.offset(offset).drawOver(lines)
    }
  }
}

object Graph {
  def from(path: Path): Graph = {
    val sourceGraph = simple(path.head.source.name)
    make(sourceGraph, path)
  }

  @tailrec
  private def make(sourceGraph: Graph, path: Path): Graph =
    path match {
      case ParentOf(p1A, c1, p1BOpt) +: ChildOf(c2, p2A, p2BOpt) +: restPath =>
        assert(p1BOpt.contains(p2A))
        assert(p2BOpt.contains(p1A))
        val targetGraph = if (restPath.isEmpty) simple(p2A.name) else from(restPath)
        parentOfChildOf(sourceGraph, c1.name, targetGraph)
      case ChildOf(c1, p1A, p1BOpt) +: ParentOf(p2A, c2, p2BOpt) +: restPath if p1BOpt == p2BOpt =>
        val graph = childOfParentOf(sourceGraph, p1BOpt.map(_.name), p1A.name, c2.name)
        make(graph, restPath)
      case ParentOf(pA, c, pBOpt) +: restPath =>
        val graph = parentOf(sourceGraph, pBOpt.map(_.name), c.name)
        make(graph, restPath)
      case ChildOf(c, pA, pBOpt) +: restPath =>
        val graph = childOf(sourceGraph, pBOpt.map(_.name), pA.name)
        make(graph, restPath)
      case _ => assert(path.isEmpty); sourceGraph
    }

  private def simple(name: String): Graph = {
    val nameLabel = Drawable.OnPath.name(name)
    val label: Positioned[Drawable] = nameLabel.topLeftAt(Position(0, 0))
    new Graph(label, label)(label)
  }

  private def parentOfChildOf(sourceGraph: Graph, childName: String, targetGraph: Graph): Graph = {
    val MinSubGraphsDistance = 3

    val parentsLine = math.max(sourceGraph.targetLabel.rectangle.top, targetGraph.sourceLabel.rectangle.top)
    val parentToParentLink = Drawable.OnPath.parentToParentLink(
      minLength =
        sourceGraph.dimensions.width - sourceGraph.targetLabel.rectangle.right +
          MinSubGraphsDistance +
          targetGraph.sourceLabel.rectangle.left
    )

    val sourceAlignment = Position(
      line = parentsLine,
      column = sourceGraph.targetLabel.rectangle.right,
    )
    val targetAlignment = sourceAlignment + Offset.columns(parentToParentLink.dimensions.width)

    val childLink = Drawable.OnPath.childLink(linkToLeft = true, linkToRight = true)
    val child = Drawable.OnPath.child(childName)

    val positionedSourceGraph = sourceGraph.topLeftAt(sourceAlignment - sourceGraph.targetLabel.rectangle.topRight.asOffset)
    val positionedTargetGraph = targetGraph.topLeftAt(targetAlignment - targetGraph.sourceLabel.rectangle.topLeft.asOffset)
    val positionedParentToParentLink = parentToParentLink.topLeftAt(positionedSourceGraph.targetLabel.rectangle.topRight)
    val positionedChildLink = childLink.topLeftAt(positionedParentToParentLink.rectangle.topMiddle)
    val positionedChild = child.topMiddleAt(positionedChildLink.rectangle.bottomLeft)

    new Graph(positionedSourceGraph.sourceLabel, positionedTargetGraph.targetLabel)(
      positionedSourceGraph,
      positionedTargetGraph,
      positionedParentToParentLink,
      positionedChildLink,
      positionedChild,
    )
  }

  private def childOfParentOf(sourceGraph: Graph, leftParentNameOption: Option[String], rightParentName: String, rightChildName: String): Graph = {
    val ChildrenDistance = 3

    val leftParentAndLink = leftParentNameOption match {
      case Some(leftParentName) => Drawable.OffPath.leftParentAndLink(leftParentName)
      case None => Drawable.nothing
    }
    val rightParentAndLink = Drawable.OnPath.rightParentAndLink(rightParentName)
    val leftChildLink = Drawable.OnPath.childLink(linkToLeft = leftParentNameOption.isDefined, linkToRight = true)
    val rightChildLink = Drawable.OnPath.childLink(linkToLeft = true, linkToRight = true)
    val rightChild = Drawable.OnPath.name(rightChildName)

    val alignment = Position(
      line = math.max(sourceGraph.targetLabel.rectangle.top, leftChildLink.dimensions.height),
      column = math.max(sourceGraph.targetLabel.rectangle.middleColumn, leftParentAndLink.dimensions.width)
    )

    val positionedSourceGraph = sourceGraph.topLeftAt(alignment - sourceGraph.targetLabel.rectangle.topMiddle.asOffset)
    val positionedLeftChildLink = leftChildLink.bottomLeftAt(positionedSourceGraph.targetLabel.rectangle.topMiddle)
    val positionedLeftParentAndLink = leftParentAndLink.topRightAt(positionedLeftChildLink.rectangle.topLeft)
    val positionedRightChild = rightChild.topLeftAt(positionedSourceGraph.targetLabel.rectangle.topRight + Offset.columns(ChildrenDistance))
    val positionedRightChildLink = rightChildLink.bottomLeftAt(positionedRightChild.rectangle.topMiddle)
    val positionedRightParentAndLink = rightParentAndLink.topLeftAt(positionedRightChildLink.rectangle.topRight)
    val positionedLinkLine = Drawable.OnPath.horizontalLine(positionedRightChildLink.rectangle.left - positionedLeftChildLink.rectangle.right)
      .topLeftAt(positionedLeftChildLink.rectangle.topRight)

    new Graph(positionedSourceGraph.sourceLabel, positionedRightChild)(
      positionedSourceGraph,
      positionedLeftChildLink,
      positionedLeftParentAndLink,
      positionedLinkLine,
      positionedRightParentAndLink,
      positionedRightChildLink,
      positionedRightChild,
    )
  }

  private def parentOf(sourceGraph: Graph, rightParentNameOption: Option[String], childName: String): Graph = {
    val leftParentLink = Drawable.OnPath.leftParentLink
    val rightParentAndLink = rightParentNameOption match {
      case Some(rightParentName) => Drawable.OffPath.rightParentAndLink(rightParentName)
      case None => Drawable.nothing
    }
    val childLink = Drawable.OnPath.childLink(linkToLeft = true, linkToRight = rightParentNameOption.isDefined)
    val child = Drawable.OnPath.child(childName)

    val positionedSourceGraph = sourceGraph.topLeftAt(Position(0, 0))
    val positionedLeftParentLink = leftParentLink.topLeftAt(sourceGraph.targetLabel.rectangle.topRight)
    val positionedChildLink = childLink.topLeftAt(positionedLeftParentLink.rectangle.topRight)
    val positionedRightParentAndLink = rightParentAndLink.topLeftAt(positionedChildLink.rectangle.topRight)
    val positionedChild = child.topMiddleAt(positionedChildLink.rectangle.bottomLeft)

    new Graph(positionedSourceGraph.sourceLabel, positionedChild)(
      positionedSourceGraph,
      positionedLeftParentLink,
      positionedRightParentAndLink,
      positionedChildLink,
      positionedChild,
    )
  }

  private def childOf(sourceGraph: Graph, leftParentNameOption: Option[String], rightParentName: String): Graph = {
    val leftParentAndLink = leftParentNameOption match {
      case Some(leftParentName) => Drawable.OffPath.leftParentAndLink(leftParentName)
      case None => Drawable.nothing
    }
    val rightParentLink = Drawable.OnPath.rightParentLink
    val rightParent = Drawable.OnPath.name(rightParentName)
    val childLink = Drawable.OnPath.childLink(linkToLeft = leftParentNameOption.isDefined, linkToRight = true)

    val alignment = Position(
      line = math.max(sourceGraph.targetLabel.rectangle.topMiddle.line, childLink.dimensions.height),
      column = math.max(sourceGraph.targetLabel.rectangle.topMiddle.column, leftParentAndLink.dimensions.width),
    )

    val positionedSourceGraph = sourceGraph.topLeftAt(alignment - sourceGraph.targetLabel.rectangle.topMiddle.asOffset)
    val positionedChildLink = childLink.bottomLeftAt(positionedSourceGraph.targetLabel.rectangle.topMiddle)
    val positionedLeftParentAndLink = leftParentAndLink.topRightAt(positionedChildLink.rectangle.topLeft)
    val positionedRightParentLink = rightParentLink.topLeftAt(positionedChildLink.rectangle.topRight)
    val positionedRightParent = rightParent.topLeftAt(positionedRightParentLink.rectangle.topRight)

    new Graph(positionedSourceGraph.sourceLabel, positionedRightParent)(
      positionedSourceGraph,
      positionedChildLink,
      positionedLeftParentAndLink,
      positionedRightParentLink,
      positionedRightParent,
    )
  }

  private implicit class PositionedGraphOps(private val pg: Positioned[Graph]) extends AnyVal {
    def sourceLabel: Positioned[Drawable] =
      pg.drawable.sourceLabel.offset(pg.topLeftPosition.asOffset)

    def targetLabel: Positioned[Drawable] =
      pg.drawable.targetLabel.offset(pg.topLeftPosition.asOffset)
  }
}
