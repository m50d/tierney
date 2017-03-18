package tierney.free

import org.junit.Test
import cats.syntax.functor._
import cats.syntax.flatMap._
import cats.Monad
import fs2.Task
import cats.~>
import cats.Id
import org.junit.Assert.assertEquals

sealed trait MyCommand[A] {
  def value: A
}

case class Delay() extends MyCommand[Unit] {
  override def value = {}
}

class TierneyFreeTest {
  @Test def basicFunctionality(): Unit = {
    val myCommand = for {
      _ <- Serial(Delay())
      _ <- Serial(Delay())
    } yield 5
    
    val result = myCommand.runSerialOrUnprincipled(Lambda[MyCommand ~> Id](_.value))
    
    assertEquals(5, result)
  }
  
}