import java.util.concurrent._
import scala.util.DynamicVariable

val forkJoinPool = new ForkJoinPool

val scheduler =
  new DynamicVariable[TaskScheduler](new DefaultTaskScheduler)

def task[T](body: => T): ForkJoinTask[T] = {
  scheduler.value.schedule(body)
}

def parallel[A, B](taskA: => A, taskB: => B): (A, B) = {
  scheduler.value.parallel(taskA, taskB)
}

def parallel[A, B, C, D](taskA: => A, taskB: => B, taskC: => C, taskD: => D): (A, B, C, D) = {
  val ta = task { taskA }
  val tb = task { taskB }
  val tc = task { taskC }
  val td = taskD
  (ta.join(), tb.join(), tc.join(), td)
}

abstract class TaskScheduler {
  def schedule[T](body: => T): ForkJoinTask[T]
  def parallel[A, B](taskA: => A, taskB: => B): (A, B) = {
    val right = task {
      taskB
    }
    val left = taskA
    (left, right.join())
  }
}

class DefaultTaskScheduler extends TaskScheduler {
  def schedule[T](body: => T): ForkJoinTask[T] = {
    val t = new RecursiveTask[T] {
      def compute = body
    }
    Thread.currentThread match {
      case wt: ForkJoinWorkerThread =>
        t.fork()
      case _ =>
        forkJoinPool.execute(t)
    }
    t
  }
}

import scala.math._


// Sum of power of array segment
def power(x: Int, p: Double): Int = exp(p * log(abs(x))).toInt

def sumSegment(a: Array[Int], p: Double, s: Int, t: Int): Int = {
  var i = s; var sum = 0
  while (i < t) {
    sum = sum + power(a(i), p)
    i = i + 1
  }
  sum
}

sumSegment(Array(1, 2, 3, 4, 5, 6), 2, 0, 3)

// p-norm Sequential
def pNorm(a: Array[Int], p: Double): Int = power(sumSegment(a, p, 0, a.length), 1/p)
pNorm(Array(1, 2, 3, 4, 5, 6), 2)

// To do parallel programming first think how the work can be divided up
def pNorm2(a: Array[Int], p: Double): Int = {
  val m = a.length / 2
  val (sum1, sum2) = (sumSegment(a, p, 0, m), sumSegment(a, p, m, a.length))
  power(sum1 + sum2, 1/p)
}
pNorm2(Array(1, 2, 3, 4, 5, 6), 2)

// p-norm Parallel
def pNormParallel(a: Array[Int], p: Double): Int = {
  val m = a.length / 2
  val (sum1, sum2) = parallel(sumSegment(a, p, 0, m), sumSegment(a, p, m, a.length))
  power(sum1 + sum2, 1/p)
}
pNormParallel(Array(1, 2, 3, 4, 5, 6), 2)

// p-norm Unbounded parallel threads
//def pNormRec(a: Array[Int], p: Double): Int = power(segmentRec(a, p, 0, a.length), 1/p)

// Parallel sum segment
//def segmentRec(a: Array[Int], p: Double, s: Int, t: Int): Int = {
//  if (t - s < threshold) sumSegment(a, p, s, t)
//  else {
//    val m = s + (t - s) / 2
//    val (sum1, sum2) = parallel(segmentRec(a, p, s, m), segmentRec(a, p, m, t))
//    sum1 + sum2
//  }
//}