package com.github.knshiro
package euler

/**
 * Created by knshiro on 7/6/14.
 */
class Problem2 {
  def fibonacciNaive(n: Int): Int = n match {
    case 0 => 0
    case 1 => 1
    case n => fibonacciNaive(n - 1) + fibonacciNaive(n - 2)
  }

  def fibonacciRecursiveDynamic(n: Int): Long = {
    val cache: Array[Long] = Array.fill(n + 1)(-1)
    cache(0) = 0
    cache(1) = 1
    def fr(i: Int): Long = {
      println("I: " + i)
      if (cache(i) < 0) {
        cache(i) = fr(i - 1) + fr(i - 2)
      }
      cache(i)
    }
    fr(n)
  }

  def fibonacciImperative(n: Int): Long = {
    var curr: Long = 2
    var previous: Long = 1
    for (i <- 3 to n) {
      curr = curr + previous
      previous = curr - previous
    }
    curr
  }

  def fibonacciImperativeArray(n: Int): Long = {
    val res: Array[Long] = Array.ofDim(n + 1)

    res(0) = 0
    res(1) = 1
    res(2) = 2

    var curr: Long = res(2)
    var previous: Long = res(1)
    for (i <- 3 to n) {
      curr = curr + previous
      previous = curr - previous
      res(i) = curr
    }
    res(n)
  }

  def fibFrom(a: Long, b: Long): Stream[Long] = a #:: fibFrom(b, a + b)

  def fibonacciFunctional: Stream[Long] = {
    //lazy val fibs: Stream[Long] = 0L #:: fibs.scanLeft(1L)(_ + _)
    //lazy val fibs: Stream[Long] = 1L #:: 2L #:: (fibs zip fibs.tail).map { t => t._1 + t._2}

    fibFrom(1,2)
  }


  def fibonacciImperativeInf(n: Int): Long = {
    var curr: Long = 2
    var previous: Long = 1
    while (curr <= n) {
      curr = curr + previous
      previous = curr - previous
    }
    previous
  }

  def fibonacciImperativeSumEvenInf(n: Int): Long = {
    var curr: Long = 2
    var previous: Long = 1
    var sum:Long = 0
    while (curr <= n) {
      curr = curr + previous
      previous = curr - previous
      if (previous % 2 == 0) {
        sum += previous
      }
    }
    sum
  }

  def fibonacciFunctionalInf(n: Int): Long = {
    fibonacciFunctional takeWhile (_ <= n) last
  }

  def fibonacciFunctionalSumEvenInf(n: Int): Long = {
    fibonacciFunctional takeWhile (_ <= n) filter (_ % 2 == 0) sum
  }


}

object Problem2 extends Problem2 with Problem {
  def run() {
    val LIMIT = 4000000
    /*
    //println(fibonacciNaive(LIMIT))
    //println("fibonacciRecursiveDynamic(LIMIT)")
    //println(fibonacciRecursiveDynamic(LIMIT))
    println("fibonacciImperative(LIMIT)")
    println(timeAverage(fibonacciImperative(LIMIT)))
    println("fibonacciImperativeArray(LIMIT)")
    println(timeAverage(fibonacciImperativeArray(LIMIT)))
    println("fibonacciFunctional(LIMIT)")
    println(time(fibonacciFunctional drop(LIMIT-1) head))          */


/*    println("fibonacciImperativeInf(LIMIT)")
    println(timeAverage(fibonacciImperativeInf(LIMIT)))
    println("fibonacciFunctionalInf(LIMIT)")
    println(timeAverage(fibonacciFunctionalInf(LIMIT)))*/

    println("fibonacciImperativeSumEvenInf(LIMIT)")
    println(timeAverage(fibonacciImperativeSumEvenInf(LIMIT)))
    println("fibonacciFunctionalSumEvenInf(LIMIT)")
    println(timeAverage(fibonacciFunctionalSumEvenInf(LIMIT)))

  }
}
