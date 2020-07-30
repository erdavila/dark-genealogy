package dark

object Index {

  val ParentOfMap: Map[Person, Set[Relation]] =
    People.ParentOfs
      .groupMapReduce(_.source)(parentOf => Set(parentOf: Relation))(_ `union` _)

  val ChildOfMap: Map[Person, Set[Relation]] =
    People.ParentOfs
      .map(_.inverted)
      .groupMapReduce(_.source)(Set(_))(_ `union` _)

  val PeopleByName: Map[String, Person] =
    People.Aliases ++
      (ParentOfMap.keySet ++ ChildOfMap.keySet).map(person => person.name -> person)
}
