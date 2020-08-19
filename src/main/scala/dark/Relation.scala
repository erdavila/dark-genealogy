package dark

sealed abstract class Relation(val source: Person, val target: Person) {
  def inverted: Relation
}

object Relation {
  case class ParentOf(parent: Person, child: Person, otherParent: Option[Person]) extends Relation(parent, child) {
    override def inverted: Relation = ChildOf(child, parent, otherParent)
  }

  case class ChildOf(child: Person, parent: Person, otherParent: Option[Person]) extends Relation(child, parent) {
    override def inverted: Relation = ParentOf(parent, child, otherParent)
  }

  def parentsOf(parent1: Person, parent2: Person)(child: Person, moreChildren: Person*): Set[ParentOf] =
    for {
      c <- moreChildren.toSet + child
      (pA, pB) <- Set((parent1, parent2), (parent2, parent1))
    } yield ParentOf(pA, c, Some(pB))

  def parentOf(parent: Person)(child: Person, moreChildren: Person*): Set[ParentOf] =
    for {
      c <- moreChildren.toSet + child
    } yield ParentOf(parent, c, None)
}
