package karazin.scala.users.group.week2.homework

import scala.annotation.targetName
import scala.math.{abs, signum}

object Homework:

  class Rational(x: Int, y: Int):
    require(y > 0, "Denominator must be positive")

    def this(x: Int) = this(x, 1)

    val numer = x / g
    val denom = y / g

    @targetName("addition")
    infix def +(that: Rational): Rational =
      new Rational(this.numer * that.denom + that.numer * this.denom, this.denom * that.denom)

    @targetName("negation")
    infix def unary_- : Rational =
      new Rational(-this.numer, this.denom)

    @targetName("substraction")
    infix def -(that: Rational): Rational =
      this + (-that)

    @targetName("multiplication")
    infix def *(that: Rational): Rational =
      new Rational(this.numer * that.numer, this.denom * that.denom)

    @targetName("devision")
    infix def /(that: Rational): Rational =
      new Rational(this.numer * that.denom, this.denom * that.numer)

    override def toString: String = s"${this.numer}/${this.denom}"

    private def gcd(a: Int, b: Int): Int =
      if b == 0 then a else gcd(b, a % b)

    private lazy val g = gcd(abs(x), y)

    override def equals(other: Any): Boolean = other match
      case that: Rational => this.numer == that.numer && this.denom == that.denom
      case _ => false

  end Rational

end Homework
