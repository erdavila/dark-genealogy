package dark

import dark.Path.Direction
import dark.display.Sentences

object Main {

  def main(args: Array[String]): Unit =
    args.toList match {
      case character1 :: character2 :: Nil =>
        def personEither(character: String) =
          Index.PeopleByName.get(character)
            .toRight(s"Character ${quoted(character)} is invalid")

        val personPairEither = for {
          p1 <- personEither(character1)
          p2 <- personEither(character2)
        } yield (p1, p2)

        personPairEither match {
          case Left(errorMessage) => System.out.println(errorMessage)
          case Right((person1, person2)) =>
            showPath(person1, person2, Direction.Down)
            println()
            showPath(person1, person2, Direction.Up)
        }

      case "--list" :: _ =>
        Index.PeopleByName
          .groupMap(_._2.name)(_._1)
          .toSeq
          .sortBy(_._1)
          .foreach { case (name, variations) =>
            val aliases = variations.filter(_ != name).toList
            val aliasesText = aliases match {
              case Nil => ""
              case alias :: Nil => " Alias: " + quoted(alias)
              case _ => " Aliases: " + aliases.sorted.map(quoted).mkString(", ")
            }
            println(quoted(name) + aliasesText)
          }

      case _ =>
        println("Use arguments:")
        println("  <character> <character>")
        println("  --list")
    }

  private def quoted(s: String) =
    s""""$s""""

  private def showPath(from: Person, to: Person, direction: Direction): Unit = {
    val directionText = direction match {
      case Direction.Down => " down"
      case Direction.Up => " up"
      case Direction.All => ""
    }
    println(s"From ${from.name}$directionText to ${to.name}")
    Path.shortest(direction)(from, to) match {
      case Some(path) => Sentences.get(path).foreach(sentence => println(s"  $sentence"))
      case None => println("  Path not found")
    }
  }
}
