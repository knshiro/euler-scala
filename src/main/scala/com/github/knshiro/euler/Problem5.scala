package com.github.knshiro
package euler

import annotation.tailrec

/**
 * Created by knshiro on 7/6/14.
 */
class Problem5 {

  def divides(k:Int,n:Int) = n % k == 0

  def ldf(k: Int, n: Int): Int = {
    if (divides(k, n)) k
    else if ((k * k) > n) n
    else ldf((k + 1), n)
  }

  def dividablesImperative(n:Int):Int = {
    var l:List[Int] = Nil
    for{
      i <- 2 to n
    } {
      val k = l.foldLeft(i){ (acc, x) =>
        val res = if(divides(x,acc)) acc / x else acc
        res
      }
      if(k>1) l = k +: l
    }
    l.fold(1)(_ * _)
  }

  def dividablesFunctional(n:Int):Int = {

    @tailrec
    def recur(range:List[Int], acc:List[Int]):List[Int] = (range,acc) match {
      case (Nil,acc) => acc
      case (h::tail,l) =>
        val rest = l.foldLeft(h){ (acc, x) =>
          val res = if(divides(x,acc)) acc / x else acc
          res
        }
        recur(tail, rest::acc)
    }
    recur(List.range(2,n),Nil).fold(1)(_ * _)
  }

}

object Problem5 extends Problem5 with Problem {

  def run() = {

    val NUMBER = 20

    println("JVM warming")
    timeAverage(dividablesImperative(NUMBER),100,false)

    println(s"dividablesImperative($NUMBER)")
    println(timeAverage(dividablesImperative(NUMBER)))

    println(s"dividablesFunctional($NUMBER)")
    println(timeAverage(dividablesFunctional(NUMBER)))
  }
}
