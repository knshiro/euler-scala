package com.github.knshiro
package s99

import scala.annotation.tailrec

class Problem4 {
  def lengthImp[T](s:Seq[T]):Int = {
    var i = 0
    var curr = s
    while (!curr.isEmpty) {
      curr = curr.tail
      i += 1
    }
    i
  }

  def lengthRec[T](l:Seq[T]):Int = {
    @tailrec
    def rec(s:Seq[T], acc:Int):Int = s match {
      case Nil => acc
      case s => rec(s.tail, acc+1)
    }
    rec(l,0)
  }

  // The previous implementation should be easily replaced with a fold
  def lengthFoldLeft[T](l:Seq[T]):Int = l.foldLeft(0){ (acc, _) => acc + 1 }

  // It happens to be more than 2x slower than the manually implemented recursion

  // Let's try foldRight even if it's almost never recommended
  def lengthFoldRight[T](l:Seq[T]):Int = l.foldRight(0){ (_, acc) => acc + 1 }

  // It's indeed 3x times slower
}

object Problem4 extends Problem4 with Problem {
  
  def run() = {
    val l = List.fill(1000000)(0)

    assert(l.size == lengthImp(l))
    assert(l.size == lengthRec(l))
    assert(l.size == lengthFoldLeft(l))
    assert(l.size == lengthFoldRight(l))

    println("warmup")
    timeAverage(l.size, 200, false)
    timeAverage(lengthImp(l), 200, false)
    timeAverage(lengthRec(l), 200, false)
    timeAverage(lengthFoldLeft(l), 200, false)
    timeAverage(lengthFoldRight(l), 200, false)

    println("l.size")
    timeAverage(l.size, 1000)
    println("lengthImp(l)")
    timeAverage(lengthImp(l), 1000)
    println("lengthRec(l)")
    timeAverage(lengthRec(l), 1000)
    println("lengthFoldLeft(l)")
    timeAverage(lengthFoldLeft(l), 1000)
    println("lengthFoldRight(l)")
    timeAverage(lengthFoldRight(l), 1000)
  }

}
