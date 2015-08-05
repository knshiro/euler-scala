package com.github.knshiro
package s99

import scala.annotation.tailrec

class Problem2 {

  def penultimateImp[T](l:Seq[T]):T = {
    var curr = l
    while(!curr.tail.tail.isEmpty){
      curr = curr.tail
    }
    curr.head
  }

  @tailrec
  final def penultimateRec[T](l:Seq[T]):T = l match {
    case x1 +: x2 +: Nil => x1
    case x +: xs => penultimateRec(xs)
    case _ => throw new NoSuchElementException
  }

  // Performance is quite bad (more than 200x slower) despite the tailrec optimization, let's try
  // another unapply method
  @tailrec
  final def penultimateRec2[T](l:Seq[T]):T = l match {
    case Seq(x1, x2) => x1
    case _ +: xs => penultimateRec2(xs)
    case _ => throw new NoSuchElementException
  }

  // Much better (only 1.5x slower) but let's see if the last unapply has an impact
  @tailrec
  final def penultimateRec3[T](l:Seq[T]):T = l match {
    case Seq(x1, x2) => x1
    case x if !x.isEmpty => penultimateRec3(x.tail)
    case _ => throw new NoSuchElementException
  }

  // Indeed it did close the gap with the imperative version!
}

object Problem2 extends Problem2 with Problem {
  
  def run() = {
    val list = List.range(1,100000)
    println(list.last)
    println(penultimateImp(list))
    println(penultimateRec(list))
    println(penultimateRec2(list))
    println(penultimateRec3(list))
    println("warm up")
    timeAverage(penultimateImp(list),200, doPrint = false)
    timeAverage(penultimateRec(list),200, doPrint = false)
    timeAverage(penultimateRec2(list),200, doPrint = false)
    timeAverage(penultimateRec3(list),200, doPrint = false)

    timeAverage(penultimateImp(list),1000)
    timeAverage(penultimateRec(list),1000)
    timeAverage(penultimateRec2(list),1000)
    timeAverage(penultimateRec3(list),1000)
  }

}
