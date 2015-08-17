package com.github.knshiro
package s99

import scala.annotation.tailrec
import scala.reflect.ClassTag

class Problem6 {

  def isPalindrome(s:String):Boolean = {
    val l = s.size
    val half = math.floor(l/2) 
    var i = 0
    var res = true
    while(i < half && res) {
      if(s(i) != s(l-i-1)) { res = false }
      i += 1
    }
    res
  }

  // Make a recursive version of this is pretty hard and gave me a complicated
  // method
  def isPalindromeRec(s:String):Boolean = {
    val l = s.size

    @tailrec
    def check(begin:List[Char],end:List[Char]):Boolean = (begin, end) match {
      case (Nil, _) | (_, Nil) => true
      case (x::xs, y::ys) => x == y && check(xs,ys)
    }

    @tailrec
    def rec(begin:List[Char], end:List[Char], count:Int):Boolean = (begin,end,count) match {
      case (begin, end, count) if count == math.floor(l/2) => if (l % 2 == 0) check(begin,end) else check(begin, end.tail)
      case (xs, y::ys, count) => rec(y::xs, ys, count+1)
    }

    rec(Nil, s.toList,0)
  }
  // This is pretty slow too (around 5x slower)

  // A simple and elegant way is to just testing the equality of the string and
  // its reverse
  def isPalindromeReverse(s:String):Boolean = {
    s.reverse == s
  }
  // It's a one liner and only 2x slower

  // You can optimize by only checking half of the string
  def isPalindromeReverse2(s:String):Boolean = {
    val l = s.size
    if(s.isEmpty || l == 1 ) true // the second check prevents the method to crash at the last line
    else {
      val (begin,end) = s.splitAt(math.floor(l/2).toInt)
      if (l % 2 == 0) begin == end.reverse else begin == end.tail.reverse
    }
  }
  // There we get a similar speed than the imperative version.

}

object Problem6 extends Problem6 with Problem {

  def run() = {
    assert(isPalindrome("abcd") == false)
    assert(isPalindrome("abcde") == false)
    assert(isPalindrome("abba") == true )
    assert(isPalindrome("abbba") == true)

    assert(isPalindromeRec("abcd") == false)
    assert(isPalindromeRec("abcde") == false)
    assert(isPalindromeRec("abba") == true )
    assert(isPalindromeRec("abbba") == true)

    assert(isPalindromeReverse("abcd") == false)
    assert(isPalindromeReverse("abcde") == false)
    assert(isPalindromeReverse("abba") == true )
    assert(isPalindromeReverse("abbba") == true)

    assert(isPalindromeReverse2("abcd") == false)
    assert(isPalindromeReverse2("abcde") == false)
    assert(isPalindromeReverse2("abba") == true )
    assert(isPalindromeReverse2("abbba") == true)

    val nonPalindrom = ('a' to 'z').mkString * 100
    val palindrom = nonPalindrom + nonPalindrom.reverse
    println("isPalindrome")
    timeAverage(isPalindrome(palindrom), 1000, true)

    println("isPalindromeRec")
    timeAverage(isPalindromeRec(palindrom), 1000, true)

    println("isPalindromeReverse")
    timeAverage(isPalindromeReverse(palindrom), 1000, true)

    println("isPalindromeReverse2")
    timeAverage(isPalindromeReverse2(palindrom), 1000, true)

  }

}
