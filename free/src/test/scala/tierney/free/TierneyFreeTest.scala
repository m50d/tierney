package tierney.free

import org.junit.Test
import cats.syntax.functor._
import cats.syntax.flatMap._
import cats.syntax.apply._
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
  val nothingInterpreter = Lambda[MyCommand ~> Id](_.value)
  
  @Test def basicFunctionality(): Unit = {
    val myCommand = for {
      _ <- Serial(Delay())
      _ <- Serial(Delay())
    } yield 5
    
    val result = myCommand.runSerialOrUnprincipled(nothingInterpreter)
    
    assertEquals(5, result)
  }
  
  @Test def parallel(): Unit = {
    val myCommand = (Parallel(Delay()).map2(Parallel(Delay())){
      (_, _) => 2
    })
    
    val result = myCommand.runSerialOrUnprincipled(nothingInterpreter)
    
    assertEquals(2, result)
  }
  
}