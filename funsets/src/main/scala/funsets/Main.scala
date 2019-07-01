package funsets

object Main extends App {
  import FunSets._
  println(contains(singletonSet(1), 1))

  def positives(x: Int) : Boolean =
    if (x > 0) true
    else false

  def empty(x: Int) : Boolean = false

  println(contains(union(positives, empty), -2))

}
