

val s = "Hello world"
s flatMap (c => List('.', c))

val ints = List(1, 2, 3, 4)
ints flatMap (int => List(int*int))

ints map (y => (1, y))

def isPrime(n : Int) : Boolean = {
  (2 to n - 1) forall(x => n % x != 0)
}

isPrime(6)