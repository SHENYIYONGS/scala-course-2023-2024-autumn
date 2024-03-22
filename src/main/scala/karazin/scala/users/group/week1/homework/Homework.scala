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

object Homework:

  object `Boolean Operators`:

    def not(b: Boolean): Boolean = if (b) false else true

    def and(left: Boolean, right: Boolean): Boolean = if (left) right else false

    def or(left: Boolean, right: Boolean): Boolean = if (left) true else right

  end `Boolean Operators`

  object `Fermat Numbers`:

    @tailrec
    def addition(a: BigInt, b: BigInt, acc: BigInt = 0): BigInt =
      if (b == 0) acc else addition(a, b - 1, acc + a)

    @tailrec
    def power(base: BigInt, exp: BigInt, acc: BigInt = 1): BigInt =
      if (exp == 0) acc else power(base, exp - 1, addition(acc, base))

    def fermatNumber(n: Int): BigInt = power(BigInt(2), BigInt(2).pow(n)) + 1

  end `Fermat Numbers`

  object `Look-and-say Sequence`:

    def lookAndSaySequenceElement(n: Int): BigInt = {
      @tailrec
      def nextElement(current: String, count: Int = n): String =
        if (count == 0) current
        else {
          val next = current.foldRight(List.empty[(Char, Int)]) {
            case (char, (lastChar, lastCount) :: tail) if char == lastChar => (char, lastCount + 1) :: tail
            case (char, acc) => (char, 1) :: acc
          }.map { case (char, count) => s"$count$char" }.mkString("")
          nextElement(next, count - 1)
        }

      if (n <= 0) 1 else BigInt(nextElement("1"))
    }

  end `Look-and-say Sequence`

end Homework
