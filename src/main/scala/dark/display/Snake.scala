package dark.display

import dark.Path.Path

object Snake {
  def get(path: Path): Seq[String] =
    snake.Snake.from(path)
}
