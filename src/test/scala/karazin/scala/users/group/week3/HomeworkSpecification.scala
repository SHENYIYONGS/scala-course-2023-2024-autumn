package karazin.scala.users.group.week3

import scala.math._
import org.scalacheck._
import org.scalacheck.Prop.forAll
import Homework._

object NatSpecification extends Properties("Nat") {

  property("Addition is commutative") = forAll { (n1: Int, n2: Int) =>
    (!(n1 >= 0 && n2 >= 0)) || {
      val nat1 = Zero.fromInt(n1)
      val nat2 = Zero.fromInt(n2)
      (nat1 + nat2).toInt == (nat2 + nat1).toInt
    }
  }

  property("Subtraction non-negative result") = forAll { (n1: Int, n2: Int) =>
    (!(n1 >= n2 && n2 >= 0)) || {
      val nat1 = Zero.fromInt(n1)
      val nat2 = Zero.fromInt(n2)
      (nat1 - nat2).toInt == n1 - n2
    }
  }
}



end NatSpecification
