package tierney.core

import cats.~>
import org.junit.Test
import org.junit.Assert.assertEquals

sealed trait ListF[G[_], A]
case class NilF[G[_], A]() extends ListF[G, A]
case class ConsF[G[_], A](head: A, tail: G[A]) extends ListF[G, A]
object ListF {
  implicit object ListFFunctor1 extends Functor1[ListF] {
    def map[G[_], H[_]](f: G ~> H): ListF[G, ?] ~> ListF[H, ?] =
      Lambda[ListF[G, ?] ~> ListF[H, ?]](_ match {
          case NilF() => NilF()
          case ConsF(head, tail) => ConsF(head, f(tail))
        })
    }
}

class Fix1Test {
  type List_[A] = Fix1[ListF, A] 
  def nil[A]: List_[A] = NilF[List_, A]()
  def cons[A](head: A, tail: List_[A]): List_[A] = ConsF(head, tail)
  val exampleList = cons(2, cons(1, nil))
  
  @Test def cata(): Unit = {
    val list = exampleList.cata[List](Lambda[ListF[List, ?] ~> List](_ match {
      case NilF() => Nil
      case ConsF(head, tail) => head :: tail
    }))
    assertEquals(List(2, 1), list)
  }
}