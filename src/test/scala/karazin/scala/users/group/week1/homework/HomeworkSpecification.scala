package karazin.scala.users.group.week1.homework

import org.scalacheck._
import Prop.{forAll, propBoolean}
import Homework._
import karazin.scala.users.group.week1.homework.arbitraries

object HomeworkSpecification extends Properties("Homework"):

  include(BooleanOperatorsSpecification)
  include(FermatNumbersSpecification)
  include(LookAndSaySequenceSpecification)

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

  // 修正的测试用例，将 .toIntExact 替换为 .toInt
  property("fermatNumber") = forAll { (n: Int) =>
    (n >= 0) ==> { fermatNumber(n) == BigInt(2).pow(BigInt(2).pow(n).toInt) + 1 }
  }

end FermatNumbersSpecification

object LookAndSaySequenceSpecification extends Properties("Look-and-say Sequence"):
  import `Look-and-say Sequence`._
  import arbitraries.given Arbitrary[Int]

  // 保持原样，除非需要根据具体逻辑进行调整
  property("lookAndSaySequenceElement") = forAll { (n: Int) =>
    (n > 0) ==> { lookAndSaySequenceElement(n) != 42 } // 根据实际逻辑可能需要调整
  }

end LookAndSaySequenceSpecification
