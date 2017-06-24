import scala.collection.GenSeq

def initialiseArray(xs: Array[Int])(v: Int): Unit ={
  for (i <- xs.indices.par) xs(i) = v
}

val xs = Array(0, 0, 0, 0)
initialiseArray(xs)(2)
xs(0)
xs(xs.length - 1)

// Other data parallel operations
// In Scala most collection operations can become data-parallel
(1 until 1000).par
  .filter(n => n % 3 == 0)
  .count(n => n.toString == n.toString.reverse) // count 3 digit palindromes

// Non-parallelisable operations
def sum(xs: Array[Int]): Int = xs.par.foldLeft(0)(_ + _)
// similarly foldRight, scanLeft, scanRight all execute sequentially

// fold operation is the answer to the above problem
def sum2(xs: Array[Int]): Int = xs.par.fold(0)(_ + _)
def max(cs: Array[Int]): Int = xs.par.fold(Int.MinValue)(Math.max)
// where Math.max is same as (x,y) => if (x>y) x else y

def isVowel(c: Char): Boolean = "aeiou" contains c.toLower
//Array('E', 'P', 'F', 'L').par.fold(0)(
//  (count, c) => if (isVowel(c)) count + 1 else count
//)
// The above program does not even compile neutral needs to be a Char
// foldLeft operation provided arbitrary accumulation types
// Although foldLeft can not run in parallel, it is more expressive

// Alternative is aggregate
Array('E', 'P', 'F', 'L').par.aggregate(0)(
  (count, c) => if (isVowel(c)) count + 1 else count,
  _ + _
)
