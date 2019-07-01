object examples {
  def sum(f: Int => Int, a: Int, b: Int) : Int =
    if (a > b) 0
    else f(a) + sum(f, a + 1, b)

  def sumInts(a:Int, b:Int) =
    sum(x=>x, a, b)

  def sumCubes(a:Int, b:Int) =
    sum(x=>x*x, a, b)

  def sumFactorials(a:Int, b:Int) =
    sum(fact, a, b)


  def product(f: Int => Int)( a: Int, b: Int) : Int =
    if (a > b) 1
    else f(a) * product(f)(a+1, b)

  def product2(f: Int => Int) : (Int, Int) => Int = {
    def prod(a: Int, b:Int) : Int =
      if (a > b) 1
      else f(a) * prod(a+1, b)
    prod
  }

  def fact(x:Int) : Int =
    product2(x=>x)(1, 5)


  def generalize(f: Int => Int)(f2: (Int, Int) => Int)(a: Int, b:Int) : Int =
    if (a > b) f2(1, -1) * -1
    else f2(f(a), generalize(f)(f2)(a+1, b))



}
examples.generalize(x=>x)((x,y)=>x*y)(2,4)
//examples.product(x=>x)(2, 4)
//examples.product2(x=>x)(2,4)
//examples.fact(4)
//examples.sumCubes(2, 2)