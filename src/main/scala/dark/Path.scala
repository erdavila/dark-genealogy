package dark

object Path {

  type Path = Vector[Relation]

  sealed trait Direction
  object Direction {
    case object Up extends Direction
    case object Down extends Direction
    case object All extends Direction
  }

  def shortest(direction: Direction)(from: Person, to: Person): Option[Path] = {
    def extendPath(path: Path, candidateRelations: Iterable[Relation], peopleInPath: Set[Person]): Option[Path] = {
      val pathCompletedWithJustOneMoreRelation = candidateRelations
        .find(_.target == to)
        .map(path :+ _)

      lazy val pathCompletedWithMoreThanOneMoreRelation = candidateRelations
        .filter(rel => !peopleInPath.contains(rel.target))
        .map(rel =>
          extendPath(
            path = path :+ rel,
            candidateRelations = Relations.all(rel.target).filter(_ != rel.inverted),
            peopleInPath = peopleInPath + rel.target
          )
        )
        .collect { case Some(p) => p }
        .minByOption(_.length)

      pathCompletedWithJustOneMoreRelation `orElse` pathCompletedWithMoreThanOneMoreRelation
    }

    val candidateRelations = direction match {
      case Direction.Up => Relations.childOfs(from)
      case Direction.Down => Relations.parentOfs(from)
      case Direction.All => Relations.all(from)
    }

    extendPath(Vector.empty, candidateRelations, Set(from))
  }

  private object Relations {
    def childOfs(child: Person): Set[Relation] = Index.ChildOfMap.getOrElse(child, Set.empty)
    def parentOfs(parent: Person): Set[Relation] = Index.ParentOfMap.getOrElse(parent, Set.empty)
    def all(person: Person): Set[Relation] = childOfs(person) ++ parentOfs(person)
  }
}
