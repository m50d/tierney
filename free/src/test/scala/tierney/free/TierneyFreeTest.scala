package tierney.free

import org.junit.Test
import cats.instances.int._
import cats.syntax.apply._
import cats.~>
import cats.Id
import org.junit.Assert.assertEquals
import tierney.parallel.ParallelApplicative
import cats.Monad
import scala.annotation.tailrec

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
  implicit def monadCostEstimate: Monad[CostEstimate] =
    new Monad[CostEstimate] {
      override def pure[A](a: A) = CostEstimate(0, a)
      override def flatMap[A, B](fa: CostEstimate[A])(f: A ⇒ CostEstimate[B]) = {
        val b = f(fa.value)
        CostEstimate(fa.cost + b.cost, b.value)
      }
      override def tailRecM[A, B](a: A)(f: A ⇒ CostEstimate[Either[A, B]]): CostEstimate[B] = {
        @tailrec def go(current: CostEstimate[Either[A, B]]): CostEstimate[B] = current match {
          case CostEstimate(cost, Left(a)) ⇒
            val CostEstimate(nextCost, nextValue) = f(a)
            go(CostEstimate(cost + nextCost, nextValue))
          case CostEstimate(cost, Right(b)) ⇒ CostEstimate(cost, b)
        }
        go(f(a))
      }
    }
  implicit def parallelApplicativeCostEstimate: ParallelApplicative[CostEstimate] =
    new ParallelApplicative[CostEstimate] {
      override def pure[A](a: A) = CostEstimate(0, a)
      override def ap[A, B](ff: CostEstimate[A ⇒ B])(fa: CostEstimate[A]) =
        CostEstimate(ff.cost max fa.cost, ff.value(fa.value))
    }
}

class TierneyFreeTest {
  val nothingInterpreter = Lambda[MyCommand ~> Id](_.value)
  val costInterpreter = Lambda[MyCommand ~> CostEstimate] { c ⇒ CostEstimate(1, c.value) }

  def serialRepeat(command: Serial[MyCommand, Unit]): Serial[MyCommand, Unit] =
    for {
      x ← command
      y ← command
      z ← command
    } yield {}

  def parallelRepeat(command: Parallel[MyCommand, Unit]): Parallel[MyCommand, Unit] =
    catsSyntaxTuple3Semigroupal[Parallel[MyCommand, ?], Unit, Unit, Unit]((command, command, command)) mapN {
      (_, _, _) ⇒
    }

  @Test def serialNothing(): Unit =
    assertEquals({}, serialRepeat(Serial(Delay())).runSerialOrUnprincipled(nothingInterpreter))

  @Test def serialCost(): Unit =
    assertEquals(CostEstimate(3, {}), serialRepeat(Serial(Delay())).runParallel(costInterpreter))

  @Test def parallelNothing(): Unit =
    assertEquals({}, parallelRepeat(Parallel(Delay())).runSerialOrUnprincipled(nothingInterpreter))

  @Test def parallelCost(): Unit =
    assertEquals(CostEstimate(1, {}),  parallelRepeat(Parallel(Delay())).runParallel(costInterpreter))

  @Test def parallelCostRunSerial(): Unit =
    assertEquals(CostEstimate(3, {}),  parallelRepeat(Parallel(Delay())).runSerialOrUnprincipled(costInterpreter))

  @Test def nesting(): Unit = {
    val interleavedCommand = serialRepeat(parallelRepeat(serialRepeat(parallelRepeat(Parallel(Delay())).serial).parallel).serial)
    assertEquals(CostEstimate(9, {}), interleavedCommand.runParallel(costInterpreter))
    assertEquals(CostEstimate(81, {}), interleavedCommand.runSerialOrUnprincipled(costInterpreter))
  }

  val countCommands = Lambda[MyCommand ~> Lambda[A => Int]](_ => 1)
  
  @Test def shallowAnalyze(): Unit = {
    assertEquals(1, serialRepeat(Serial(Delay())).shallowAnalyze(countCommands))
    assertEquals(3, parallelRepeat(Parallel(Delay())).shallowAnalyze(countCommands))
  }
  
  @Test def shallowAnalyzeDeeplyNested(): Unit = {
    assertEquals(3, parallelRepeat(
        Parallel(Delay())).node.parallel.serial.node.parallel.serial.node.parallel.serial.shallowAnalyze(countCommands))
  }
}