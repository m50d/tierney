package tierney.free

import org.junit.Test
import cats.syntax.functor._
import cats.syntax.flatMap._
import cats.syntax.apply._
import fs2.Task
import cats.~>
import cats.Id
import org.junit.Assert.assertEquals
import tierney.parallel.ParallelApplicative
import cats.Monad
import scala.annotation.tailrec
import org.junit.Ignore

sealed trait MyCommand[A] {
  def value: A
}

case class Delay() extends MyCommand[Unit] {
  override def value = {}
}

/** Pseudo-writer with "optimized" applicative behaviour
 *  to estimate the costs it would take to run a given command chain
 *  with "perfect" parallelism
 */
case class CostEstimate[A](cost: Int, value: A)
object CostEstimate {
  implicit def instanceCostEstimate: ParallelApplicative[CostEstimate] with Monad[CostEstimate] =
    new ParallelApplicative[CostEstimate] with Monad[CostEstimate] {
      override def pure[A](a: A) = CostEstimate(0, a)
      override def ap[A, B](ff: CostEstimate[A ⇒ B])(fa: CostEstimate[A]) =
        CostEstimate(ff.cost max fa.cost, ff.value(fa.value))
      override def flatMap[A, B](fa: CostEstimate[A])(f: A ⇒ CostEstimate[B]) = {
        val b = f(fa.value)
        CostEstimate(fa.cost + b.cost, b.value)
      }
      override def tailRecM[A, B](a: A)(f: A => CostEstimate[Either[A,B]]): CostEstimate[B] = {
        @tailrec def go(current: CostEstimate[Either[A, B]]): CostEstimate[B] = current match {
          case CostEstimate(cost, Left(a)) =>
            val CostEstimate(nextCost, nextValue) = f(a)
            go(CostEstimate(cost + nextCost, nextValue))
          case CostEstimate(cost, Right(b)) => CostEstimate(cost, b)
        }
        go(f(a))
      }
    }
}

class TierneyFreeTest {
  val nothingInterpreter = Lambda[MyCommand ~> Id](_.value)
  val costInterpreter = Lambda[MyCommand ~> CostEstimate]{c => CostEstimate(1, c.value)}
  
  val serialCommand = for {
    _ ← Serial(Delay())
    _ ← Serial(Delay())
  } yield 5

  val parallelCommand = (Parallel(Delay()).map2(Parallel(Delay())) {
    (_, _) ⇒ 2
  })

  @Test def serialNothing(): Unit =
    assertEquals(5, serialCommand.runSerialOrUnprincipled(nothingInterpreter))
    
  @Test def serialCost(): Unit =
    assertEquals(CostEstimate(2, 5), serialCommand.runParallel(costInterpreter))

  @Test def parallelNothing(): Unit =
    assertEquals(2, parallelCommand.runSerialOrUnprincipled(nothingInterpreter))
    
  @Ignore
  @Test def parallelCost(): Unit =
    assertEquals(CostEstimate(1, 2), parallelCommand.runParallel(costInterpreter))
    
  @Test def parallelCostRunSerial(): Unit =
    assertEquals(CostEstimate(2, 2), parallelCommand.runSerialOrUnprincipled(costInterpreter))

  @Test def nesting(): Unit = {

  }

}