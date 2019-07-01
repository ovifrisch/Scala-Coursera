

def intersect(xs : List[Int], ys : List[Int]) : List[Int] = {
  val y_map = ys.map(y => (y, true)).toMap
  xs.filter(x =>  y_map.contains(x))
}

def intersect2(xs : List[Int], ys : List[Int]) : List[Int] = {
  xs.filter(x => ys.exists(y => y == x))
}


def my_filter[T] (xs : List[T], p : T => Boolean) : List[T] = {
  xs match {
    case Nil => Nil
    case head::tail => {
      if (p(head)) head :: my_filter(tail, p)
      else my_filter(tail, p)
    }
  }
}

def get_time(f: => Any) = {
  val start = System.currentTimeMillis()
  val x = f
  val end = System.currentTimeMillis()
  "time: " + (end - start) + "ms"
}

def test() = {
  val xs = 1 to 10000 toList
  val ys = 20000 to 1 by -1 toList
  val x = get_time(intersect(xs, ys))
  val y = get_time(intersect2(xs, ys))
}

val x = ('a' to 'z').toList.map(x => (x, x.toInt)) toMap
val ys = x.mkString("\t")






