package com.github.knshiro
package euler

/**
 * Created by knshiro on 7/6/14.
 */
class Problem1 {
  def imperatifForArray(n: Int) = {
    val a = Array.range(1, n)
    var sum = 0
    for (i <- a) {
      if (i % 3 == 0 || i % 5 == 0) {
        sum += i
      }
    }
    sum
  }

  def imperatifForFilteredArray(n: Int) = {
    val a: Array[Int] = Array.range(1, n) filter (x => x % 5 == 0 || x % 3 == 0)
    var sum = 0
    for (i <- a) {
      sum += i
    }
    sum
  }

  def imperatifForFilteredManualArray(n: Int) = {
    val arr: Array[Int] = new Array[Int](n)
    var index = 0
    while(index < n-1){
      arr(index) = index+1
      index += 1
    }

    val a = arr filter (x => x % 5 == 0 || x % 3 == 0)
    var sum = 0
    for (i <- a) {
      sum += i
    }
    sum
  }

  def imperatifWhileFilteredArray(n: Int) = {
    val a: Array[Int] = Array.range(1, n) filter (x => x % 5 == 0 || x % 3 == 0)
    var sum = 0
    var i = 0
    while (i < a.length) {
      sum += a(i)
      i += 1
    }
    sum
  }

  def imperatifSum(n: Int) = {
    var i = 1;
    var sum = 0;
    while (i < n) {
      if (i % 3 == 0 || i % 5 == 0) {
        sum += i
      }
      i += 1
    }
    sum
  }

  def arrayFilteredSum(n: Int) = {
    (Array.range(1, n-1) filter (x => x % 5 == 0 || x % 3 == 0)).sum
  }

  def forComprehensionSum(n: Int) = {
    (for (i <- 1 until n if i % 5 == 0 || i % 3 == 0) yield i).sum
  }

  def rangeFilteredSum(n: Int) = {
    ((1 until n) filter (x => x % 5 == 0 || x % 3 == 0)).sum
  }

}

object Problem1 extends Problem1 with Problem{

  def run() {
    val LIMIT = 1000
    println("timeAverage(imperatifSum(LIMIT))")
    println(timeAverage(imperatifSum(LIMIT)))
    println("timeAverage(imperatifForArray(LIMIT))")
    println(timeAverage(imperatifForArray(LIMIT)))
    println("timeAverage(imperatifWhileFilteredArray(LIMIT))")
    println(timeAverage(imperatifWhileFilteredArray(LIMIT)))
    println("timeAverage(arrayFilteredSum(LIMIT))")
    println(timeAverage(arrayFilteredSum(LIMIT)))
    println("timeAverage(forComprehensionSum(LIMIT))")
    println(timeAverage(forComprehensionSum(LIMIT)))
    println("timeAverage(rangeFilteredSum(LIMIT))")
    println(timeAverage(rangeFilteredSum(LIMIT)))
    println("timeAverage(imperatifForFilteredArray(LIMIT))")
    println(timeAverage(imperatifForFilteredArray(LIMIT)))
    println("timeAverage(imperatifForFilteredManualArray(LIMIT))")
    println(timeAverage(imperatifForFilteredManualArray(LIMIT)))
  }

}
