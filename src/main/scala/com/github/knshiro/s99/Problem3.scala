package com.github.knshiro
package s99

import scala.annotation.tailrec

class Problem3 {

  def nth[T](n:Int, s:Seq[T]):T = {
    s.drop(n).head
  }

  def nthImp[T](n:Int, s:Seq[T]):T = {
    var curr = s
    for(i <- 1 to n) {
      curr = curr.tail
    }
    curr.head
  }

  // this is twice slower than reference implementation
  // Let's change the for loop to a while
  
  def nthImp2[T](n:Int, s:Seq[T]):T = {
    var curr = s
    var i = 0
    while (i < n) {
      curr = curr.tail
      i += 1
    }
    curr.head
  }

  // I guess creating the range took as much time as going through the list
  // itself.

}

object Problem3 extends Problem3 with Problem {
  
  def run() = {
    val list = List.range(1,1000000)
    val k = 100000
    assert(list(k) == nth(k,list))
    assert(list(k) == nthImp(k,list))
    assert(list(k) == nthImp2(k,list))

    println("warm up")
    timeAverage(list(k),200, doPrint = false) 
    timeAverage(nth(k,list),200, doPrint = false) 
    timeAverage(nthImp(k,list),200, doPrint = false) 
    timeAverage(nthImp2(k,list),200, doPrint = false) 
    
    timeAverage(list(k),1000) 
    timeAverage(nth(k,list),1000) 
    timeAverage(nthImp(k,list),1000) 
    timeAverage(nthImp2(k,list),1000) 
  }

}
