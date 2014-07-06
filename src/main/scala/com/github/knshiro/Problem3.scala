package com.github.knshiro

/**
 * Created by knshiro on 7/6/14.
 */
class Problem3 {

  // Adapted from http://www.vogella.com/tutorials/JavaAlgorithmsPrimeFactorization/article.html
  def findHighestPrimeFactorImperatif(n: Long): Seq[Long] = {

    var factors: List[Long] = List.empty[Long]
    var curr = n
    var i = 2L

    while (i < curr / i) {
      while (curr % i == 0) {
        factors = i +: factors;
        curr /= i;
      }
      i += 1
    }
    if (curr > 1) {
      factors = curr +: factors;
    }
    factors
  }


  // Adapted from http://louisbotterill.blogspot.jp/2009/03/prime-factorization-comparison-between.html
  def findHighestPrimeFactorFunctional(n: Long): Seq[Long] = {
    def divides(d: Long, n: Long) = {
      (n % d) == 0
    }

    def ld(n: Long): Long = {
      ldf(2, n)
    }

    def ldf(k: Long, n: Long): Long = {
      if (divides(k, n)) k
      else if ((k * k) > n) n
      else ldf((k + 1), n)
    }

    def factors(n: Long, i:Long): List[Long] = n match {
      case 1 => Nil;
      case _ => {
        val p = ldf(i,n)
        p :: factors(n / p, i)
      }
    }

    factors(n, 2)
  }
}

object Problem3 extends Problem3 with Problem {
  def run() = {
    val NUMBER = 600851475143L

    println(findHighestPrimeFactorImperatif(10))
    println(findHighestPrimeFactorFunctional(10))

    println()

    println(findHighestPrimeFactorImperatif(16))
    println(findHighestPrimeFactorFunctional(16))

    println()

    println("findHighestPrimeFactorImperatif(NUMBER)")
    println(timeAverage(findHighestPrimeFactorImperatif(NUMBER)))
    println("findHighestPrimeFactorFunctional(NUMBER)")
    println(timeAverage(findHighestPrimeFactorFunctional(NUMBER)))

  }
}
