package dark.display

import dark.Path.Path
import dark.Relation.{ChildOf, ParentOf}
import dark.{Gender, Person}

object Sentences {
  def get(path: Path): Seq[String] = {
    val pathSource = path.head.source
    val pathTarget = path.last.target

    def sentence(source: Person, role: String, target: Person, complement: Option[(String, Person, String)]) = {
      def formattedPerson(color: String, bold: Boolean, name: String) =
        s"$color${if (bold) Console.BOLD else ""}$name${Console.RESET}"

      val formattedSource = formattedPerson(Console.GREEN, source == pathSource, source.name)
      val formattedTarget = formattedPerson(Console.YELLOW, target == pathTarget, target.name)
      val formattedComplement = complement.fold("") { case (prefix, person, suffix) =>
        s" $prefix${Console.BLUE}${person.name}${Console.RESET}$suffix"
      }

      s"$formattedSource is $role of $formattedTarget$formattedComplement"
    }


    path.map {
      case ParentOf(parent, child, otherParent) =>
        val role = parent.gender match {
          case Gender.Male => "father"
          case Gender.Female => "mother"
        }
        val complement = otherParent.map(p => ("(with ", p, ")"))
        sentence(parent, role, child, complement)

      case ChildOf(child, parent, otherParent) =>
        val role = child.gender match {
          case Gender.Male => "son"
          case Gender.Female => "daughter"
        }
        val complement = otherParent.map(p => ("(and ", p, ")"))
        sentence(child, role, parent, complement)
    }
  }
}
