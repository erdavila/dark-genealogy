package dark

import dark.Relation.ParentOf
import scala.Ordering.Implicits.seqOrdering

object Main {
  private implicit val personOrdering: Ordering[Person] =
    Ordering.by(_.name)

  private implicit def relationOrdering[R <: Relation]: Ordering[R] =
    Ordering.by(_.target)

  private implicit def personSetOrdering[A: Ordering]: Ordering[Set[A]] =
    Ordering.by(_.to(Seq).sorted)

  def main(args: Array[String]): Unit = {
    println("Parents")
    for ((child, parents) <- Index.ChildOfMap.to(Seq).sorted) {
      println(s"  ${child.name}: ${parents.to(Seq).map(_.target.name).sorted.mkString(", ")}")
    }

    println()

    println("Children")
    for ((parent, relations) <- Index.ParentOfMap.to(Seq).sorted) {
      println(s"  ${parent.name}:")
      for (relation@ParentOf(_, _, _) <- relations.to(Seq).sorted) {
        println(s"    ${relation.target.name}" + relation.otherParent.fold("")(p => s" (with ${p.name})"))
      }
    }
  }
}
