package karazin.scala.users.group.week3

object Homework:
  
  // Peano numbers
  abstract class Nat:
    def isZero: Boolean
    def predecessor: Nat
    def successor: Nat = Succ(this)
    
    infix def + (that: Nat): Nat
    
    infix def - (that: Nat): Nat
    
    // Optional task
    def toInt: Int
    
    // Optional task
    def fromInt(int: Int) =
      if (int <= 0) Zero
    else Succ(fromInt(int - 1))



    override def toString: String = s"Nat($predecessor)"
  
  type Zero = Zero.type 
  object Zero extends Nat:
    def isZero: Boolean = true
    def predecessor: Nat = throw new Exception("0 doesn't have a predecessor")
    
    infix def +(that: Nat): Nat = that
    
    infix def -(that: Nat): Nat =
      if (that.isZero) this
      else throw new Exception("Negative number result")



    // Optional task
    def toInt: Int = 0

    override def toString: String = "Zero"
    override def equals(obj: Any): Boolean =
      obj.isInstanceOf[Zero.type] // Zero is equal to Zero



  class Succ(n: Nat) extends Nat:
    def isZero: Boolean = false
    def predecessor: Nat = n
    
    infix def +(that: Nat): Nat =  successor + that
    
    infix def -(that: Nat): Nat =
      if (that.isZero) this
      else predecessor - that.predecessor



    // Optional task
    def toInt: Int = 1 + predecessor.toInt

    override def equals(obj: Any): Boolean = obj match {
      case s: Succ => s.predecessor == this.predecessor
      case _ => false
    }


