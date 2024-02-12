package karazin.scala.users.group.week1.homework

import scala.annotation.tailrec
/**
 * Preface
 * Implement all the things with ???.
 * All implementations must be tail-recursive is possible.
 * Feel free to use internal methods/functions.
 * Feel free to adjust signatures of hacked methods/functions.
 *
 * 1. Boolean Operators
 * Required task
 * Implement eager boolean operations Not, And and Or.
 * Requirements:
 * a) the function should not use embedded boolean operations
 * b) the functions should be eager
 * c) the function should use `if` expression and `true` and `false` boolean literals 
 *
 * 2. Fermat Numbers
 * Required task
 * Implement function which should calculate n-th Fermat number:
 * Fn = 2 ^ (2 ^ n) + 1, n is non-negative
 * Requirements:
 * a) the function should not use multiplication and power operations
 * b) the functions should only use addition operation
 * For more details @see https://en.wikipedia.org/wiki/Fermat_number
 *
 * 3. Look-and-say Sequence
 * Required task
 * Implement function which should calculate n-th number of Look-and-say sequence
 * For more details @see https://en.wikipedia.org/wiki/Look-and-say_sequence
 *
 * 4. Kolakoski sequence
 * Optional super challenging task
 * Implement function which should calculate n-th number of Kolakoski sequence
 * For more details @see https://en.wikipedia.org/wiki/Kolakoski_sequence
 */

object Homework :

  object `Boolean Operators` :

    val int = 42

    def not(b: Boolean): Boolean ={
      if (b) false else true
    }

    // here is my greatest solution

    def and(left: Boolean, right: Boolean): Boolean =  {
      if (left) right else false
    }



    def or(left: Boolean, right: Boolean): Boolean = {
      if (left) true else right
    }



  end `Boolean Operators`

  object `Fermat Numbers` :

    val multiplication: (BigInt, BigInt) => BigInt = (a, b) => {
      if (a == 0 || b == 0) 0
      else a + multiplication(a, b - 1)
    }



    val power: (BigInt, BigInt) => BigInt =  (base, exp) => {
      if (exp == 0) 1
      else base * power(base, exp - 1)
    }



    val fermatNumber: Int => BigInt =  n => {
      if (n == 0) 0
      else power(2, power(2, n - 1)) + 1
    }



  end `Fermat Numbers`

  object `Look-and-say Sequence` :
    val lookAndSaySequenceElement: Int => BigInt = n => {
      def generateNext(s: String, count: Int, acc: String): String = {
        if (s.isEmpty) acc + count + s.head
        else if (s.head == acc.last) generateNext(s.tail, count + 1, acc)
        else generateNext(s.tail, 1, acc + count + s.head)
      }

      def lookAndSayHelper(current: String, iterations: Int): String = {
        if (iterations == 0) current
        else lookAndSayHelper(generateNext(current, 1, ""), iterations - 1)
      }

      BigInt(lookAndSayHelper("1", n))
    }



  end `Look-and-say Sequence`

end Homework