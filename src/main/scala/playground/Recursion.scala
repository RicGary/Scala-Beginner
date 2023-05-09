package lectures.part1basics

import scala.annotation.tailrec
import scala.jdk.Accumulator

object Recursion extends App {
  def factorial(n: Int): Int =
    if (n <= 1) 1
    else {
      println("Computing factorial of " + n + " - I first need factorial of " + (n - 1))
      val result = n * factorial(n - 1)
      println("Computed factorial of " + n)

      result
    }

  //  println(factorial(5000)) // stack overflow!

  def anotherFactorial(n: Int): BigInt = {
    @tailrec
    def factHelper(x: Int, accumulator: BigInt): BigInt =
      if (x <= 1) accumulator
      else factHelper(x - 1, x * accumulator) // TAIL RECURSION = use recursive call as the LAST expression

    factHelper(n, 1)
  }

  /*
    Breakdown:

    anotherFactorial(10) = factHelper(10, 1)
    = factHelper(9, 10 * 1)
    = factHelper(8, 9 * 10 * 1)
    = factHelper(7, 8 * 9 * 10 * 1)
    = ...
    = factHelper(2, 3 * 4 * ... * 10 * 1)
    = factHelper(1, 1 * 2 * 3 * 4 * ... * 10)
    = 1 * 2 * 3 * 4 * ... * 10
   */

  //println(anotherFactorial(20000))

  // WHEN YOU NEED LOOPS, USE _TAIL_ RECURSION.

  /*
      Exercises:
      1.  Concatenate a string n times
      2.  IsPrime function tail recursive
      3.  Fibonacci function, tail recursive.
  */

  // 2 IsPrime

  def isPrime(n: Int): Boolean = {
    def isPrimeUntil(t: Int): Boolean =
      if (t <= 1) true
      else n % t != 0 && isPrimeUntil(t - 1)

    isPrimeUntil(n / 2)
  }

  def anotherIsPrime(n: BigInt): Boolean = {
    @tailrec
    def isPrimeUntil(t: BigInt, isStillPrime: Boolean): Boolean =
      if(!isStillPrime) false
      else if (t <= 1) true
        // %
      else isPrimeUntil(t - 1, n % t != 0 && isStillPrime)

    isPrimeUntil(n / 2, isStillPrime = true)
  }
  /*
      false and true -> false
  5, 10 % 5 != 0 and true -> false
  10 % 4 != 0 and false

  */

  def FibTailRec(n: Int): Int = {
    @tailrec  /*           n - 1           n - 2                    */
    def accFib(i: Int, last: Int, nextToLast: Int): Int =
      if (i >= n) last
      else {
        println("-" * 5)
        println("N: " + n + "\nI: " + i)
        accFib(i + 1, last + nextToLast, last)
      }

    if (n <= 2) 1
    else accFib(2, 1, 1)
  }
  /*
  Fibb we got n-1 + n-2
  In tail recursion we have the numbers of return in a normal fuction as qtd. of params
  */
  println(FibTailRec(8))
}