import scala.collection.{GenSeq, GenSet, mutable}
// Scala parallel collections
/*
Traits ParIterable[T], ParSeq[T], ParSet[T] and ParMap[K, V] are the parallel counterparts
of different sequential traits.

For code that is agnostic about parallelism, there exists a separate hierarchy of generic
collection traits GenIterable[T], GenSeq[T], GenSet[T] and GenMap[K, V].
 */
// Transformers such as map, filter, flatMap, groupBy can all run in parallel

// Generic  collection traits allow us to write parallelism agnostic code
// Largest palindrome
def largestPalindrome(xs: GenSeq[Int]):Int = {
  xs.aggregate(Int.MinValue)(
    (largest, n) => if (n > largest && n.toString == n.toString.reverse) n else largest,
    math.max
  )
}
largestPalindrome((0 until 1000000).toArray.par)

/*
NOTE: unless conversion to a parallel collection takes a negligible amount of time compared
to subsequent parallel operations, then pick your data structures carefully and make sure
that they are parallelisable.
 */

// Parallelisable collections
/*
- ParArray[T] – parallel array of objects, counterpart of Array and ArrayBuffer
- ParRange – parallel range of integers, counterpart of Range
- ParVector[T] – parallel vector, counterpart of Vector
- immutable.ParHashSet[T] – counterpart of immutable.HashSet
- immutable.ParHashMap[K, V] – counterpart of immutable.HashMap
- mutable.ParHashSet[T] – counterpart of mutable.HashSet
- mutable.PasHashMap[K, V] – counterpart of mutable.HashMap
- ParTrieMap[K, V] – thread-safe parallel map with atomic snapshots, counterpart of TrieMap
- for other collections, par creates the closest parallel collection – e.g. a List is converted to a ParVector
 */

def intersection(a: GenSet[Int], b: GenSet[Int]) = {
  val result = mutable.Set[Int]()
  for (x <- a) if (b contains x) result += x
  result
}
intersection((0 until 1000).toSet, (0 until 1000 by 4).toSet)
intersection((0 until 1000).par.toSet, (0 until 1000 by 4).par.toSet)

// RULE: avoid mutations to the same memory locations without proper synchronisation.

/*
 Solution to the above problem would be to use a concurrent collection, which can be
 mutated by multiple threads.

 Side-effects can be avoided altogether by using the correct combinators. For example,
 we can use filter to compute the intersection
  */
def intersection2(a: GenSet[Int], b: GenSet[Int]): GenSet[Int] = {
  if (a.size < b.size) a.filter(b(_))
  else b.filter(a(_))
}
intersection2((0 until 1000).toSet, (0 until 1000 by 4).toSet)
intersection2((0 until 1000).par.toSet, (0 until 1000 by 4).par.toSet)