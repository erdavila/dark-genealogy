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
}
