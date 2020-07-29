package dark

import dark.Gender.{Female, Male}

object People {
  val Agnes: Person = Person("Agnes", Female)
  val Aleksander: Person = Person("Aleksander", Male)
  val Bartosz: Person = Person("Bartosz", Male)
  val Bernd: Person = Person("Bernd", Male)
  val Charlotte: Person = Person("Charlotte", Female)
  val Claudia: Person = Person("Claudia", Female)
  val Doris: Person = Person("Doris", Female)
  val Daniel: Person = Person("Daniel", Male)
  val Egon: Person = Person("Egon", Male)
  val Elisabeth: Person = Person("Elisabeth", Female)
  val Franziska: Person = Person("Franziska", Female)
  val Greta: Person = Person("Greta", Female)
  val Hannah: Person = Person("Hannah", Female)
  val Helene: Person = Person("Helene", Female)
  val Helge: Person = Person("Helge", Male)
  val Hermann: Person = Person("Hermann", Male)
  val Ines: Person = Person("Ines", Female)
  val Jana: Person = Person("Jana", Female)
  val Jonas: Person = Person("Jonas", Male)
  val Katharina: Person = Person("Katharina", Female)
  val Mads: Person = Person("Mads", Male)
  val Magnus: Person = Person("Magnus", Male)
  val Martha: Person = Person("Martha", Female)
  val Mikkel: Person = Person("Mikkel", Male)
  val Noah: Person = Person("Noah", Male)
  val Peter: Person = Person("Peter", Male)
  val Regina: Person = Person("Regina", Female)
  val Sebastian: Person = Person("Sebastian", Male)
  val Silja: Person = Person("Silja", Female)
  val TheUnknown: Person = Person("The Unknown", Male)
  val Tronte: Person = Person("Tronte", Male)
  val Ulrich: Person = Person("Ulrich", Male)

  val Aliases =
    Map(
      "Michael" -> Mikkel,
      "The Strange" -> Jonas,
      "Strange" -> Jonas,
      "Adam" -> Jonas,
      "Unknown" -> TheUnknown,
    )

  private val Kahnwald =
    parentsOf(Mikkel, Hannah)(Jonas) ++
      parentOf(Daniel)(Ines)

  private val Nielsen =
    parentsOf(Ulrich, Katharina)(Magnus, Martha, Mikkel) ++
      parentsOf(Tronte, Jana)(Ulrich, Mads)

  private val Tiedemann =
    parentsOf(Regina, Aleksander)(Bartosz) ++
      parentsOf(Claudia, Bernd)(Regina) ++
      parentsOf(Egon, Doris)(Claudia)

  private val Doppler =
    parentsOf(Charlotte, Peter)(Franziska, Elisabeth) ++
      parentOf(Helge)(Peter) ++
      parentOf(Greta)(Helge)

  val ParentOfs: Set[ParentOf] = Kahnwald ++ Nielsen ++ Tiedemann ++ Doppler ++
    parentOf(Jonas)(TheUnknown) ++
    parentsOf(TheUnknown, Agnes)(Tronte) ++
    parentsOf(Noah, Elisabeth)(Charlotte) ++
    parentsOf(Bartosz, Silja)(Noah, Agnes) ++
    parentsOf(Hannah, Egon)(Silja) ++
    parentsOf(Hermann, Helene)(Katharina) ++
    parentOf(Sebastian)(Hannah)

  type ParentOf

  private def parentsOf(parent1: Person, parent2: Person)(child: Person, moreChildren: Person*): Set[ParentOf] =
    ???

  private def parentOf(parent: Person)(child: Person, moreChildren: Person*): Set[ParentOf] =
    ???
}
