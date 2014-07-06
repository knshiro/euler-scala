package com.github.knshiro

/**
 * Created by knshiro on 7/6/14.
 */
trait Problem {
  def run(): Unit

  def time[A](a: => A) = {
    val now = System.nanoTime
    val result = a
    val micros = (System.nanoTime - now) / 1000
    println("%d microseconds".format(micros))
    result
  }

  def timeAverage[A](a: => A) = {
    val times = for (i <- 1 to 10) yield {
      val now = System.nanoTime
      val result = a
      val micros = (System.nanoTime - now) / 1000
      micros
    }
    println("Average of %d microseconds".format(times.sum / times.length))
    a
  }

}
