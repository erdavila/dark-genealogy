package dark

object Main {

  def main(args: Array[String]): Unit =
    args.toList match {
      case character1 :: character2 :: Nil =>
        println(character1)
        println(character2)

      case "--list" :: _ =>
        def quoted(s: String) = s""""$s""""
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
}
