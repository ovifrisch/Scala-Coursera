import scala.collection.immutable.Stream.Empty

abstract class IntSet {
  def incl(x: Int) : IntSet
  def contains(x: Int) : Boolean
  def union(other: IntSet) : IntSet
}

class NonEmpty(elem: Int, left: IntSet, right: IntSet) extends IntSet {
  def incl(x: Int) : IntSet = {
    if (x < elem) new NonEmpty(elem, left incl x, right)
    else if (x > elem) new NonEmpty(elem, left, right incl x)
    else this
  }

  def contains(x: Int) : Boolean = {
    if (x < elem) left contains x
    else if (x > elem) right contains x
    else true
  }

  def union(other: IntSet) : IntSet = {
    ((left union right) union other) incl elem
  }

  override def toString = "{" + left + elem + right + "}"
}

class Empty extends IntSet {
  def incl(x: Int) : IntSet = {
    new NonEmpty(x, new Empty, new Empty)
  }
  def contains(x: Int) : Boolean = false

  def union(other: IntSet) : IntSet = other
  override def toString = "."
}

val t1 = new NonEmpty(3, new Empty, new Empty)
t1 incl 4 incl 2


