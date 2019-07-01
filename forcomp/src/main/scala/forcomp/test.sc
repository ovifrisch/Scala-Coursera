
def sum(nums : List[Int]) : Int = nums.foldLeft(0) (_ + _)

// using reduce
def fact1(num : Int) : Int = (1 to num).toList.reduce (_ * _)


// using normal recursion
def fact2(num : Int) : Int = if (num == 1) 1 else num * fact2(num - 1)


// using tail recursion
def fact3(num : Int) : Int = {
  def f(num : Int, acc : Int) : Int = if (num == 1) acc else f(num-1, acc*num)
  f(num, 1)
}

def merge(nums1 : List[Int], nums2 : List[Int]) : List[Int] = {
  (nums1, nums2) match {
    case (Nil, ys) => ys
    case (xs, Nil) => xs
    case (x::xs, y::ys) => {
      if (x < y) x :: merge(xs, nums2)
      else y :: merge(nums1, ys)
    }
  }
}

def mergeSort(nums : List[Int]) : List[Int] = {
  if (nums.length == 1) nums
  else {
    val mid = nums.length / 2
    merge(mergeSort(nums.take(mid)), mergeSort(nums.drop(mid)))
  }
}

fact1(4)
fact2(4)
fact3(4)

4/3