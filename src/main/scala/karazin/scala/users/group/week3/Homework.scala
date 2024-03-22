package karazin.scala.users.group.week3

object Homework:

  // Peano numbers
  abstract class Nat:
    def isZero: Boolean
    def predecessor: Nat
    def successor: Nat = new Succ(this)

    infix def + (that: Nat): Nat

    infix def - (that: Nat): Nat

    // Optional task
    def toInt: Int

    def fromInt(int: Int): Nat =
      if int == 0 then Zero else new Succ(fromInt(int - 1))

    override def toString: String = s"Nat(${this.toInt})"

  type Zero = Zero.type
  object Zero extends Nat:
    def isZero: Boolean = true
    def predecessor: Nat = throw new Exception("0 doesn't have a predecessor")

    infix def +(that: Nat): Nat = that

    infix def -(that: Nat): Nat =
      if that.isZero then this else throw new Exception("Negative number")

    // Optional task
    def toInt: Int = 0

    override def equals(obj: Any): Boolean = obj match
      case _: Zero.type => true
      case _ => false

  class Succ(n: Nat) extends Nat:
    def isZero: Boolean = false
    def predecessor: Nat = n

    infix def +(that: Nat): Nat = new Succ(n + that)

    infix def -(that: Nat): Nat =
      if that.isZero then this else n - that.predecessor

    // Optional task
    def toInt: Int = 1 + n.toInt

    override def equals(obj: Any): Boolean = obj match
      case succ: Succ => succ.toInt == this.toInt
      case _ => false
