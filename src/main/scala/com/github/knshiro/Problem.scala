package com.github.knshiro

/**
 * Created by knshiro on 7/6/14.
 */
trait Problem {
  def run(): Unit

  def time[A](a: => A):A = {
    val now = System.nanoTime
    val result = a
    val micros = (System.nanoTime - now) / 1000
    println("%d microseconds".format(micros))
    result
  }

  def timeAverage[A](a: => A, nbTimes:Int = 10, doPrint:Boolean = true):Unit = {
    val times = for (i <- 1 to nbTimes) yield {
      val now = System.nanoTime
      val result = a
      val micros = (System.nanoTime - now) / 1000
      micros
    }
    if(doPrint) println("Average of %d microseconds".format(times.sum / times.length))
    ()
  }

}
