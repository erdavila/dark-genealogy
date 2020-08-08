package dark.display

import dark.Path.Path
import dark.Relation

object OneLine {
  def get(path: Path): String = {
    path.foldLeft(path.head.source.name) { (str, rel) =>
      rel match {
        case Relation.ParentOf(_, child, _) => s"$str \\ ${child.name}"
        case Relation.ChildOf(_, parent, _) => s"$str / ${parent.name}"
      }
    }
  }
}
