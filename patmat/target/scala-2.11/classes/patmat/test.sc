import scala.collection.mutable.Map

val chars = List('b', 'a', 'a', 'b', 'c', 'a')
val lst = List(('a',3), ('c',1), ('b',2))
lst.sortBy(_._2)

def insert(x: Int, xs: List[Int]) : List[Int] = {
  xs match {
    case Nil => List(x)
    case y :: ys => {
      if (x <= y) x :: xs
      else y :: insert(x, ys)
    }
  }
}

insert(10, List(1))