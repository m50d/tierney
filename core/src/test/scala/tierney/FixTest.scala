package tierney

import cats.derived._
import cats.derived.functor._
import cats.derived.functor.legacy._
import org.junit.Test
import org.junit.Assert.assertEquals
import cats.Functor

sealed trait IntListF[A]
case class NilF[A]() extends IntListF[A]
case class ConsF[A](head: Int, tail: A) extends IntListF[A]

class FixTest {
  type IntList = Fix[IntListF]
  val nil: IntList = Fix(NilF[IntList]())
  def cons(head: Int, tail: IntList): IntList = Fix(ConsF(head, tail))
  val exampleList = cons(2, cons(1, nil))
  
  @Test def cata(): Unit = {
    val printed = exampleList.cata[String] {
      case NilF() => ""
      case ConsF(i, s) => i + ", " + s
    }
    assertEquals("2, 1, ", printed)
  }
}