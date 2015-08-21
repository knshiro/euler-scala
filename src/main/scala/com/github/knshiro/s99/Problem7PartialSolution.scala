package com.github.knshiro
package s99

import scala.annotation.tailrec

class Problem7Incomplete {

  // Let's start with a reference implementation
  def flatten[T](l:Seq[Seq[T]]):Seq[T] = {
    l.flatMap(x => x)
  }


  // Here is a completely imperative solution
  def flattenImp[T](l:Seq[Seq[T]]):Seq[T] = {
    var res = List.empty[T]
    var curr = l
    while(!curr.isEmpty) {
      var inner = curr.head
      while(!inner.isEmpty){
        res ::= inner.head
        inner = inner.tail
      }
      curr = curr.tail
    }
    res
  }
  // It's twice as fast as the reference

  // Let's try to simplify a bit with the standard concat method
  def flattenImp2[T](l:Seq[Seq[T]]):Seq[T] = {
    var res:Seq[T] = List.empty[T]
    var curr = l
    while(!curr.isEmpty) {
      res = curr.head ++ res
      curr = curr.tail
    }
    res
  }
  // This implementation is 150x slower than the above!! I don't know what's
  // wrong with ++

  // Let's try a simple recursive solution
  def flattenRec[T](l:Seq[Seq[T]]):Seq[T] = {
    @tailrec
    def rec(l:Seq[Seq[T]], acc:Seq[T]):Seq[T] = l match {
      case Nil => acc
      case xs => rec(xs.tail, xs.head ++ acc)
    }
    rec(l,Nil)
  }
  //This is as bad as flattenImp2

  // Let's implement our own concat
  def flattenRec2[T](l:Seq[Seq[T]]):Seq[T] = {
    @tailrec
    def concat(l1:Seq[T], l2:Seq[T]):Seq[T] = l1 match {
      case Nil => l2
      case l1 => concat(l1.tail, l1.head +: l2)
    }
    @tailrec
    def rec(l:Seq[Seq[T]], acc:Seq[T]):Seq[T] = l match {
      case Nil => acc
      case xs => rec(xs.tail, concat(xs.head, acc))
    }
    rec(l,Nil)
  }
  // We get something similar to flattenImp but the concat function reverses
  // the order.

  // To get back the order we need to abandon tail recursivity
  def flattenRec3[T](l:Seq[Seq[T]]):Seq[T] = {
    def concat(l1:Seq[T], l2:Seq[T]):Seq[T] = l1 match {
      case Nil => l2
      case l1 => l1.head +: concat(l1.tail,l2)
    }
    @tailrec
    def rec(l:Seq[Seq[T]], acc:Seq[T]):Seq[T] = l match {
      case Nil => acc
      case xs => rec(xs.tail, concat(xs.head, acc))
    }
    rec(l,Nil)
  }
  // It still doesn't blow up with a List.fill(1000,1000) but is slightly
  // slower than the tail recursive solution
}

object Problem7Incomplete extends Problem7Incomplete with Problem {

  def run() = {
    val l = List.fill(100,100)(1)
    assert(flattenImp(l) == flatten(l))
    assert(flattenRec(l) == flatten(l))

    println("Warm up")
    timeAverage(flatten(l),200, false)
    timeAverage(flattenImp(l),200,false)
    timeAverage(flattenImp2(l),200,false)
    timeAverage(flattenRec(l),200,false)
    timeAverage(flattenRec2(l),200,false)

    println("flatten")
    timeAverage(flatten(l),1000, true)
    println("flattenImp")
    timeAverage(flattenImp(l),1000, true)
    println("flattenImp2")
    timeAverage(flattenImp2(l),1000, true)
    println("flattenRec")
    timeAverage(flattenRec(l),1000, true)
    println("flattenRec2")
    timeAverage(flattenRec2(l),1000, true)
    println("flattenRec3")
    timeAverage(flattenRec3(l),1000, true)

    
    println("\n\n Big list")
    val bigL = List.fill(1000,1000)(1)
    println("flattenImp")
    timeAverage(flattenImp(bigL),1000,true)
    println("flattenRec2")
    timeAverage(flattenRec2(bigL),1000, true)
    println("flattenRec3")
    timeAverage(flattenRec3(bigL),1000, true)

  }

}
