package dark

import dark.Relation.{ParentOf, parentOf, parentsOf}
import shapeless.labelled.{FieldType, field}
import shapeless.record._
import shapeless.syntax.singleton._
import shapeless.{HList, HNil, Poly1, ops}

object People {
  //noinspection TypeAnnotation
  private val PeopleInBothWorlds =
    ("Agnes"      ->> Person("Agnes", Gender.Female)) ::
      ("Aleksander" ->> Person("Aleksander", Gender.Male)) ::
      ("Bartosz"    ->> Person("Bartosz", Gender.Male)) ::
      ("Bernd"      ->> Person("Bernd", Gender.Male)) ::
      ("Charlotte"  ->> Person("Charlotte", Gender.Female)) ::
      ("Claudia"    ->> Person("Claudia", Gender.Female)) ::
      ("Doris"      ->> Person("Doris", Gender.Female)) ::
      ("Egon"       ->> Person("Egon", Gender.Male)) ::
      ("Elisabeth"  ->> Person("Elisabeth", Gender.Female)) ::
      ("Franziska"  ->> Person("Franziska", Gender.Female)) ::
      ("Greta"      ->> Person("Greta", Gender.Female)) ::
      ("Hannah"     ->> Person("Hannah", Gender.Female)) ::
      ("Helene"     ->> Person("Helene", Gender.Female)) ::
      ("Helge"      ->> Person("Helge", Gender.Male)) ::
      ("Hermann"    ->> Person("Hermann", Gender.Male)) ::
      ("Jana"       ->> Person("Jana", Gender.Female)) ::
      ("Katharina"  ->> Person("Katharina", Gender.Female)) ::
      ("Mads"       ->> Person("Mads", Gender.Male)) ::
      ("Magnus"     ->> Person("Magnus", Gender.Male)) ::
      ("Martha"     ->> Person("Martha", Gender.Female)) ::
      ("Mikkel"     ->> Person("Mikkel", Gender.Male)) ::
      ("Noah"       ->> Person("Noah", Gender.Male, List("Hanno"))) ::
      ("Peter"      ->> Person("Peter", Gender.Male)) ::
      ("Regina"     ->> Person("Regina", Gender.Female)) ::
      ("Sebastian"  ->> Person("Sebastian", Gender.Male)) ::
      ("Silja"      ->> Person("Silja", Gender.Female)) ::
      ("Tronte"     ->> Person("Tronte", Gender.Male)) ::
      ("Ulrich"     ->> Person("Ulrich", Gender.Male)) ::
      HNil

  //noinspection TypeAnnotation
  private val AdamVariantPeople = PeopleInBothWorlds.map(toAdamWorld)

  //noinspection TypeAnnotation
  private val EvaVariantPeople = PeopleInBothWorlds.map(toEvaWorld)

  private val Jonas = Person("Jonas", Gender.Male, List("The Strange", "Strange", "Adam"))
  private val TheUnknown = Person("The Unknown", Gender.Male, List("Unknown"))

  private val A = AdamVariantPeople
  private val B = EvaVariantPeople

  val All: Set[Person] = (AdamVariantPeople ::: EvaVariantPeople).values.toList.toSet + Jonas + TheUnknown

  val ParentOfs: Set[ParentOf] =
    Set.empty ++
      relationsInBothWorlds(AdamVariantPeople) ++
      relationsInBothWorlds(EvaVariantPeople) ++
      parentsOf(A("Mikkel"), A("Hannah"))(Jonas) ++
      parentsOf(Jonas, B("Martha"))(TheUnknown) ++
      parentsOf(TheUnknown, A("Agnes"))(A("Tronte")) ++
      parentsOf(TheUnknown, B("Agnes"))(B("Tronte")) ++
      Set.empty


  private type Sel[L <: HList, K] = ops.record.Selector.Aux[L, K, Person]
  private def relationsInBothWorlds[L <: HList](people: L)(
    implicit
      agnesSel:      Sel[L, "Agnes"],
      aleksanderSel: Sel[L, "Aleksander"],
      bartoszSel:    Sel[L, "Bartosz"],
      berndSel:      Sel[L, "Bernd"],
      charlotteSel:  Sel[L, "Charlotte"],
      claudiaSel:    Sel[L, "Claudia"],
      dorisSel:      Sel[L, "Doris"],
      egonSel:       Sel[L, "Egon"],
      elisabethSel:  Sel[L, "Elisabeth"],
      franziskaSel:  Sel[L, "Franziska"],
      gretaSel:      Sel[L, "Greta"],
      hannahSel:     Sel[L, "Hannah"],
      helgeSel:      Sel[L, "Helge"],
      heleneSel:     Sel[L, "Helene"],
      hermannSel:    Sel[L, "Hermann"],
      janaSel:       Sel[L, "Jana"],
      katharinaSel:  Sel[L, "Katharina"],
      madsSel:       Sel[L, "Mads"],
      magnusSel:     Sel[L, "Magnus"],
      marthaSel:     Sel[L, "Martha"],
      mikkelSel:     Sel[L, "Mikkel"],
      noahSel:       Sel[L, "Noah"],
      peterSel:      Sel[L, "Peter"],
      reginaSel:     Sel[L, "Regina"],
      siljaSel:      Sel[L, "Silja"],
      sebastianSel:  Sel[L, "Sebastian"],
      tronteSel:     Sel[L, "Tronte"],
      ulrichSel:     Sel[L, "Ulrich"],
  ): Set[ParentOf] =
    // Nielsen
    parentsOf(people("Tronte"), people("Jana"))(people("Ulrich"), people("Mads")) ++
      parentsOf(people("Ulrich"), people("Katharina"))(people("Magnus"), people("Martha"), people("Mikkel")) ++
      // Doppler
      parentOf(people("Greta"))(people("Helge")) ++
      parentOf(people("Helge"))(people("Peter")) ++
      parentsOf(people("Peter"), people("Charlotte"))(people("Franziska"), people("Elisabeth")) ++
      // Tiedemann
      parentsOf(people("Egon"), people("Doris"))(people("Claudia")) ++
      parentsOf(people("Claudia"), people("Bernd"))(people("Regina")) ++
      parentsOf(people("Regina"), people("Aleksander"))(people("Bartosz")) ++
      // others
      parentsOf(people("Noah"), people("Elisabeth"))(people("Charlotte")) ++
      parentsOf(people("Bartosz"), people("Silja"))(people("Noah"), people("Agnes")) ++
      parentsOf(people("Egon"), people("Hannah"))(people("Silja")) ++
      parentsOf(people("Hermann"), people("Helene"))(people("Katharina")) ++
      parentOf(people("Sebastian"))(people("Hannah"))


  private object toAdamWorld extends Poly1 {
    implicit def f[K]: Case.Aux[
      FieldType[K, Person],
      FieldType[K, Person]
    ] = at[FieldType[K, Person]] { person =>
      val newPerson = if (person.name == "Mikkel") {
        person.copy(aliases = "Michael" :: person.aliases)
      } else {
        person
      }
      field[K](newPerson)
    }
  }

  private object toEvaWorld extends Poly1 {
    private val EvaWorldNameSuffix = " B"

    implicit def f[K]: Case.Aux[
      FieldType[K, Person],
      FieldType[K, Person]
    ] = at[FieldType[K, Person]] { person =>
      val newName = person.name + EvaWorldNameSuffix
      val newAliases =
        person.aliases.map(_ ++ EvaWorldNameSuffix) ++
          Option.when(person.name == "Martha")("Eva")
      val newPerson = Person(newName, person.gender, newAliases)
      field[K](newPerson)
    }
  }

}
