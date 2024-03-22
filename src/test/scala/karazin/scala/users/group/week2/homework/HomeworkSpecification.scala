package karazin.scala.users.group.week2.homework

import scala.math._
import org.scalacheck._
import Prop.{forAll, propBoolean, throws}
import karazin.scala.users.group.week2.homework.arbitraries
import Homework._
import utils._

object HomeworkSpecification extends Properties("Homework"):
  import arbitraries.{given Arbitrary[Int], given Arbitrary[Rational]}

  // Existing Properties...

  property("negation") = forAll { (rational: Rational) =>
    -rational == new Rational(-rational.numer, rational.denom)
  }

  property("addition") = forAll { (left: Rational, right: Rational) =>
    left + right == new Rational(left.numer * right.denom + right.numer * left.denom, left.denom * right.denom)
  }

  property("subtraction") = forAll { (left: Rational, right: Rational) =>
    left - right == left + (-right)
  }

  property("multiplication") = forAll { (left: Rational, right: Rational) =>
    left * right == new Rational(left.numer * right.numer, left.denom * right.denom)
  }

  property("division") = forAll { (left: Rational, numer: Int, denom: Int) =>
    val right = Rational(if numer == 0 then 1 else numer, abs(denom) + 1)
    left / right == new Rational(left.numer * right.denom, left.denom * right.numer)
  }

  property("division by zero") = forAll { (left: Rational) =>
    throws(classOf[IllegalArgumentException]) {
      left / new Rational(0, 1)
    }
  }

end HomeworkSpecification
