package tierney.core

import org.junit.Test
import org.junit.Assert.assertEquals

sealed trait IntListF[A]
case class IntNilF[A]() extends IntListF[A]
case class IntConsF[A](head: Int, tail: A) extends IntListF[A]

class FixTest {
  type IntList = Fix[IntListF]
  implicit val functor = cats.derive.functor[IntListF]
  val nil: IntList = IntNilF[IntList]()
  def cons(head: Int, tail: IntList): IntList = IntConsF(head, tail)
  val exampleList = cons(2, cons(1, nil))
  
  @Test def cata(): Unit = {
    val printed = exampleList.cata[String] {
      case IntNilF() => ""
      case IntConsF(i, s) => i + ", " + s
    }
    assertEquals("2, 1, ", printed)
  }
}