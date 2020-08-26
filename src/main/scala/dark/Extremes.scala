package dark

import dark.Path.{Direction, Path}
import dark.Relation.{ChildOf, ParentOf}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}

object Extremes {

  private class Progress(total: Int) {
    private var counter = 0
    private var label = ""

    def updateLabel(): Unit = this.synchronized {
      clearLabel()
      label = s"${100 * counter / total}%"
      print(label)
    }

    def increment(): Unit = this.synchronized {
      counter += 1
      updateLabel()
    }

    def clearLabel(): Unit = this.synchronized {
      print("\u0008" * label.length)
      label = ""
    }
  }

  def findAndShow(): Unit = {
    val progress = new Progress(total = People.All.size * People.All.size * 2)

    progress.updateLabel()
    val futures = for {
      from <- People.All
      to <- People.All
      dir <- Set(Direction.Down, Direction.Up)
    } yield Future {
      val path = Path.shortest(dir)(from, to)
      progress.increment()
      path
    }

    val pathsFuture = Future.sequence(futures).map(_.flatten)
    val paths = Await.result(pathsFuture, Duration.Inf)
    progress.clearLabel()

    val extremes: Seq[(String, Option[Path])] = Seq(
      "Longest path" -> paths.maxByOption(_.length),
      "Longest path to self" -> paths.filter(path => path.head.source == path.last.target).maxByOption(_.length),
      "Shortest path to self" -> paths.filter(path => path.head.source == path.last.target).minByOption(_.length),
      "Path with more inversions" -> paths.maxByOption(
        _.grouped(2).count {
          case Vector(ParentOf(_, _, _), ChildOf(_, _, _)) => true
          case Vector(ChildOf(_, _, _), ParentOf(_, _, _)) => true
          case _ => false
        }
      ),
    )

    for ((title, pathOption) <- extremes) {
      display.show(title)(pathOption)
      println()
    }
  }
}
