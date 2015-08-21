package com.github.knshiro
package s99

import scala.annotation.tailrec

class Problem7 {

  // Let's start with a reference implementation
  def flatten(l:List[Any]):List[Any] = l flatMap {
    case seq:List[Any] => flatten(seq)
    case el => List(el)
  }
  
  // Now let's do it imperatively with variables
  def flattenImp(l:List[Any]):List[Any] = {
    var res = List.empty[Any]
    def inner(el:Any):Unit = el match {
      case list:List[Any] =>
        var curr = list
        while(!curr.isEmpty) {
          inner(curr.head)
          curr = curr.tail
        }
      case el => res ::= el
    }
    inner(l)
    res
  }
  // It's 13x faster

  // Now let's try something recursive
  def flattenRec(l:List[Any]):List[Any] = {

    def rec(l:List[Any], acc:List[Any]):List[Any] = l match {
      case Nil => acc
      case (x:List[Any])::xs => rec(xs, rec(x, acc))
      case x::xs => rec(xs, x::acc)
      case el => el::acc
    }
    rec(l,Nil)
  }
  // Surprisingly I get 30% speed increase with this solution
  // And it does not blow up even with the big list
  
  // Final comment is that I lose 30% speed if I switch all from List to Seq
}

object Problem7 extends Problem7 with Problem {

  def run() = {
    val l = List.fill(100,100)(1)
    assert(flattenImp(l) == flatten(l))
    assert(flattenRec(l) == flatten(l))

    println("Warm up")
    timeAverage(flatten(l),200, false)
    timeAverage(flattenImp(l),200,false)
    timeAverage(flattenRec(l),200,false)

    println("flatten")
    timeAverage(flatten(l),1000, true)
    println("flattenImp")
    timeAverage(flattenImp(l),1000, true)
    println("flattenRec")
    timeAverage(flattenRec(l),1000,true)
       
    println("\n\n Big list")
    val bigL = List.fill(1000,1000)(1)
    println("flattenImp")
    timeAverage(flattenImp(bigL),1000,true)
    println("flattenRec")
    timeAverage(flattenRec(bigL),1000,true)

  }

}
