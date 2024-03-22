package karazin.scala.users.group.week4.homework

import scala.annotation.targetName
import karazin.scala.users.group.week4.utils.ItemOrdering

object Homework:

  abstract class IntSet:

    infix def include(x: Int): IntSet

    infix def remove(x: Int): IntSet

    infix def contains(x: Int): Boolean

    @targetName("union")
    infix def ∪(that: IntSet): IntSet

    @targetName("intersection")
    infix def ∩(that: IntSet): IntSet

    @targetName("complement")
    infix def ∖(that: IntSet): IntSet

    @targetName("disjunctive union")
    infix def ∆(that: IntSet): IntSet

  end IntSet

  type Empty = Empty.type

  case object Empty extends IntSet:

    infix def include(x: Int): IntSet = NonEmpty(x, Empty, Empty)

    infix def contains(x: Int): Boolean = false

    infix def remove(x: Int): IntSet = this

    @targetName("union")
    infix def ∪(that: IntSet): IntSet = that

    @targetName("intersection")
    infix def ∩(that: IntSet): IntSet = this

    @targetName("complement")
    infix def ∖(that: IntSet): IntSet = this

    @targetName("disjunctive union")
    infix def ∆(that: IntSet): IntSet = that

    override def toString: String = "[*]"

    override def equals(other: Any): Boolean = other.isInstanceOf[Empty.type]

  end Empty

  case class NonEmpty(elem: Int, left: IntSet, right: IntSet) extends IntSet:

    infix def include(x: Int): IntSet =
      if x < elem       then NonEmpty(elem, left include x, right)
      else if x > elem  then NonEmpty(elem, left, right include x)
      else              this

    infix def contains(x: Int): Boolean =
      if x < elem       then left contains x
      else if x > elem  then right contains x
      else              true

    // Optional task
    infix def remove(x: Int): IntSet =
      if x < elem then NonEmpty(elem, left remove x, right)
      else if x > elem then NonEmpty(elem, left, right remove x)
      else merge(left, right)

    @targetName("union")
    infix def ∪(that: IntSet): IntSet =
      ((left ∪ right) ∪ that) include elem

    @targetName("intersection")
    infix def ∩(that: IntSet): IntSet =
      if that contains elem then (left ∩ that) ∪ (right ∩ that) include elem
      else (left ∩ that) ∪ (right ∩ that)

    @targetName("complement")
    infix def ∖(that: IntSet): IntSet =
      if that contains elem then (left ∖ that) ∪ (right ∖ that)
      else (left ∖ that) ∪ (right ∖ that) include elem

    @targetName("disjunctive union")
    infix def ∆(that: IntSet): IntSet =
      (this ∖ that) ∪ (that ∖ this)

    override def toString: String = s"[$left - [$elem] - $right]"

    override def equals(other: Any): Boolean = other match
      case that: NonEmpty => this.elem == that.elem && this.left == that.left && this.right == that.right
      case _ => false

    private def merge(a: IntSet, b: IntSet): IntSet = (a, b) match
      case (Empty, _) => b
      case (_, Empty) => a
      case (NonEmpty(x, l, r), _) => NonEmpty(x, l, merge(r, b))

  end NonEmpty

end Homework
