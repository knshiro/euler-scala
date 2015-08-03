package com.github.knshiro
package s99

import scala.annotation.tailrec

class Problem1 {

  def lastImperative[T](l:Seq[T]):T = {
    var cur = l
    while(!cur.tail.isEmpty) {
      cur = cur.tail 
    }
    cur.head
  }

  // If final is not used here, the recursion is not optimized and we get a
  // stack overflow.
  @tailrec
  final def lastRec[T](l:Seq[T]):T = l match {
    //case Seq() => throw new Exception("Empty list can not have a last element")
    case x +: Seq() => x
    case x +: xs => lastRec(xs)
  }

  // Even with tail recursion optimization there is still an order of magnitude
  // of speed difference so let's see if it comes from match
  @tailrec
  final def lastRec2[T](l:Seq[T]):T = {
    if(l.tail.isEmpty) l.head else lastRec2(l.tail)
  }

  // Removing match indeed gets us back to a similar time than the imperative
  // version. Now let's see if it is the unapply in the match that is the culprit
  @tailrec
  final def lastRec3[T](l:Seq[T]):T = l match {
    case l if l.tail.isEmpty => l.head
    case l => lastRec3(l.tail)
  }

  // Indeed the unapply was the culprit here we got performance similar to the
  // imperative implementation with lastRec3
}

object Problem1 extends Problem1 with Problem {
  
  def run() = {
    val list = List.range(1,100000)
    println("warm up")
    timeAverage(lastImperative(list),100,doPrint = false)
    timeAverage(lastRec(list),100, doPrint = false)
    timeAverage(lastRec2(list),100, doPrint = false)
    timeAverage(lastRec3(list),100, doPrint = false)

    println("lastImperative(list)")
    timeAverage(lastImperative(list),2000)
    println("lastRec(list)")
    timeAverage(lastRec(list),200)
    println("lastRec2(list)")
    timeAverage(lastRec2(list),2000)
    println("lastRec3(list)")
    timeAverage(lastRec3(list),2000)
  }
}

