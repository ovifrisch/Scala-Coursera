
import week4._

  def nth[T] (l : List[T], i : Int) : T = {
    if (i == 0) l.head
    else nth(l.tail, i - 1)
  }

  val lst = new Cons(1, new Cons(2, new Cons(3, new Nil)))
  nth(lst, 1)