package karazin.scala.users.group.week1.homework

import org.scalacheck._
import Prop.{forAll, propBoolean}
import Homework._
import karazin.scala.users.group.week1.homework.arbitraries

object HomeworkSpecification extends Properties("Homework"):

  include(BooleanOperatorsSpecification)
  include(FermatNumbersSpecification)
  include(LookAndAaSequenceSpecification)

end HomeworkSpecification

object BooleanOperatorsSpecification extends Properties("Boolean Operators"):
  import `Boolean Operators`._

  property("not") = forAll { (b: Boolean) =>
    not(b) == !b
  }

  property("and") = forAll { (pair: (Boolean, Boolean)) =>
    val (left, right) = pair

    and(left, right) == left && right
  }

  property("or") = forAll { (pair: (Boolean, Boolean)) =>
    val (left, right) = pair

    or(left, right) == left || right
  }

end BooleanOperatorsSpecification

object FermatNumbersSpecification extends Properties("Fermat Numbers"):
  import `Fermat Numbers`._
  import arbitraries.given Arbitrary[Int]

  property("multiplication") = forAll { (left: BigInt, right: BigInt) =>
    multiplication(left, right) == (left * right)
  }

  property("power") = forAll { (base: BigInt, exp: BigInt) =>
    power(base, exp) == base.pow(exp.toInt)
  }

  property("fermatNumber") = forAll { (n: Int) =>
    fermatNumber(n) == (BigInt(2).pow(BigInt(2).pow(n - 1).toInt)) + 1
  }

end FermatNumbersSpecification

object LookAndAaSequenceSpecification extends Properties("Look-and-say Sequence"):
  import `Look-and-say Sequence`._
  import arbitraries.given Arbitrary[Int]

  property("lookAndSaySequenceElement") = forAll { (n: Int) =>
    lookAndSaySequenceElement(n) == BigInt(42)
  }

end LookAndAaSequenceSpecification
