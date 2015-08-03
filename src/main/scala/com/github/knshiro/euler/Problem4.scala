package com.github.knshiro
package euler

/**
 * Created by knshiro on 7/6/14.
 */
class Problem4 {

  def palindrome(n:Int):Boolean = {
    val s = n.toString
    val r = s.reverse
    //println(s"(s,r) = ${(s,r)}")
    s == r
  }

  def minMax(n:Int):(Int,Int) = {
    val max:Int = (math.pow(10, n) - 1).toInt
    val min:Int = (math.pow(10, n-1) - 1).toInt
    (min,max)
  }

   def palindromeImperatif(n:Int):(Int,Int,Int) = {
     val (min,max) = minMax(n)

     var i = max
     var j = i
     var res = (0,0,0)
     while(i > min) {
       j = i
       while(j > min && !palindrome(i*j)){
         j -= 1
       }
       if(j > min) {
         val t = i*j
         res match {
           case (a,b,c) if c < t => res = (i,j,t)
           case _ =>
         }
       }
       i -= 1
     }
     res
   }

  def palindromeForComprehension(n:Int):(Int,Int,Int) = {
    val (min,max) = minMax(n)

    val l = for {
      i <- max to min by -1
      j <- i to min by -1 if (palindrome(i*j))
    } yield {
      (i,j,i*j)
    }
    l.maxBy(_._3)
  }

  def palindromeWithIterator(n:Int) = {
    val (min,max) = minMax(n)

    val it = Iterator.range(max,min,-1).map{ i =>
      val it = (Iterator.range(i,min,-1).dropWhile(j => !palindrome(i*j)))
      if (it.hasNext) {
        val j = it.next()
        (i,j,i*j)
      } else (0,0,0)
    }
    it.maxBy(_._3)
  }
}

object Problem4 extends Problem4 with Problem {
  def run() {
    val NUMBER = 3

    println("JVM warmup")
    timeAverage(palindromeImperatif(3),100,false)

    println(s"timeAverage(palindromeImperatif($NUMBER))")
    println(timeAverage(palindromeImperatif(NUMBER)))
    println(s"timeAverage(palindromeForComprehension($NUMBER))")
    println(timeAverage(palindromeForComprehension(NUMBER)))
    println(s"timeAverage(palindromeWithIterator($NUMBER))")
    println(timeAverage(palindromeWithIterator(NUMBER)))
  }
}
