package com.github.knshiro
package s99

import scala.annotation.tailrec
import scala.reflect.ClassTag

class Problem5 {

  def reverseImp[T](s:Seq[T]):Seq[T] = {
    var cur = s
    var res = Seq.empty[T]
    while(!cur.isEmpty) {
      res +:= cur.head
      cur = cur.tail
    }
    res
  }

  // This has similar performance to List#reverse reference implementation

  // Let's try to use a mutable Array instead
  def reverseImp2[T](s:Seq[T])(implicit arg0: ClassTag[T]):Seq[T] = {
    var cur = s
    val res = Array.ofDim[T](s.size)
    var i = res.size-1
    while(!cur.isEmpty) {
      res(i) = cur.head
      cur = cur.tail
      i -= 1
    }
    res
  }

  // It's between 10 and 50% slower, I guess that initialize the array has a cost and maybe
  // the access to the ith element is a bit slower than the head of a list.
  
  // Let's try something recursive now
  
  def reverseRec[T](s:Seq[T]):Seq[T] = {
    @tailrec
    def rec(l:Seq[T],acc:Seq[T]):Seq[T] = l match {
      case Nil => acc
      case x => rec(x.tail, x.head +: acc)
    }
    rec(s,Seq.empty[T])
  }

  // This is as fast as the reference implementation

  // Let's see now the difference with a fold
  def reverseRec2[T](s:Seq[T]):Seq[T] = {
    s.foldLeft(Seq.empty[T]){(acc,el) => el +: acc}
  }

  // The fold is 50% slower than the reference implementation.

}

object Problem5 extends Problem5 with Problem {
  
  def run() = {
    val l = Seq.range(1,100000)
    assert(l.reverse == reverseImp(l))
    assert(l.reverse == reverseImp2(l))
    assert(l.reverse == reverseRec(l))
    assert(l.reverse == reverseRec2(l))

    println("warm up")
    timeAverage(l.reverse, 200, false)
    timeAverage(reverseImp(l), 200, false)
    timeAverage(reverseImp2(l), 200, false)
    timeAverage(reverseRec(l), 200, false)
    timeAverage(reverseRec2(l), 200, false)


    println("l.reverse")
    timeAverage(l.reverse, 2000)
    println("reverseImp(l)")
    timeAverage(reverseImp(l), 2000)
    println("reverseImp2(l)")
    timeAverage(reverseImp2(l), 2000)
    println("reverseRec(l)")
    timeAverage(reverseRec(l), 2000)
    println("reverseRec2(l)")
    timeAverage(reverseRec2(l), 2000)
  }

}
