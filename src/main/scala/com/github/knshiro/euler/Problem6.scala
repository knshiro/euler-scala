package com.github.knshiro
package euler

import scala.language.postfixOps

/**
 * Created by knshiro on 7/6/14.
 */
class Problem6 {

  def sumsSquares(n:Int) = {
    List.range(1,n+1) map (i => math.pow(i,2)) sum
  }

  def squareSum(n:Int) = {
    math.pow(List.range(1L,(n+1).toLong) sum,2)
  }


  def generateSubsetsF(n: Int) = {
    @annotation.tailrec
    def gsR(l: List[Int], acc: Iterator[List[Int]]): Iterator[List[Int]] = l match {
      case Nil => acc
      case h :: tail =>
        gsR(tail, acc ++ (acc map (h +: _)))
    }

    gsR((1 to n).toList, Iterator(Nil))
  }

  def squareSumMinusSumSquareCombination(n:Int) = {
    val products = for{
      i <- 0 to n
      j <- i+1 to n
    } yield {
      i * j
    }
    2 * products.sum
  }
}

object Problem6 extends Problem6 with Problem {

  def run() = {
    val NUMBER = 100
    println("Warm JVM")

    timeAverage({squareSum(NUMBER);sumsSquares(NUMBER);squareSumMinusSumSquareCombination(NUMBER)},100,false)

    println(s"squareSum($NUMBER): ${squareSum(NUMBER)}")
    println(s"sumsSquares($NUMBER): ${sumsSquares(NUMBER)}")
    println(f"${squareSum(NUMBER) - sumsSquares(NUMBER)}%.0f")

    println(s"squareSum($NUMBER) - sumsSquares($NUMBER)")
    println(timeAverage(squareSum(NUMBER) - sumsSquares(NUMBER)))

    println(s"squareSumMinusSumSquareCombination($NUMBER)")
    println(timeAverage(squareSumMinusSumSquareCombination(NUMBER)))
  }

}
