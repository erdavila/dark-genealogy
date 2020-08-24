package dark

import dark.Path.Direction
import dark.Relation.{ChildOf, ParentOf}
import dark.display.{Graph, OneLine, Sentences, Snake}
import scala.util.Random

object Main {

  private val RandomCharacterName = "RANDOM"

  def main(args: Array[String]): Unit =
    args.toList match {
      case character1 :: character2 :: Nil => {
        def personEither(character: String): Either[String, Person] =
          if (character == RandomCharacterName) {
            val people = Index.PeopleByName.values.toSeq.distinct
            val index = Random.nextInt(people.size)
            Right(people(index))
          } else {
            Index.PeopleByName.get(character)
              .toRight(s"Character ${quoted(character)} is invalid")
          }

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
      }

      case "--list" :: Nil =>
        People.All
          .toSeq
          .sortBy(_.name)
          .foreach { person =>
            val aliasesText = person.aliases match {
              case Nil => ""
              case alias :: Nil => " Alias: " + quoted(alias)
              case _ => " Aliases: " + person.aliases.sorted.map(quoted).mkString(", ")
            }
            println(quoted(person.name) + aliasesText)
          }
        println(RandomCharacterName)

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
      case Some(path) =>
        println(s"  ${OneLine.get(path)}")
        println()
        Sentences.get(path).foreach(sentence => println(s"  $sentence"))
        println()
        Graph.get(path).foreach(line => println(s"  $line"))
        println()
        Snake.get(path).foreach(line => println(s"  $line"))
      case None => println("  Path not found")
    }
  }
}
