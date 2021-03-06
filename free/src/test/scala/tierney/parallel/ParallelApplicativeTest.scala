package tierney.parallel

import org.junit.Test
import fs2.Task
import fs2.Strategy
import fs2.interop.cats._
import cats.syntax.apply._
import tierney.free._
import org.junit.Assert.{assertTrue, assertFalse}
import scala.concurrent.Await
import scala.concurrent.duration._

class ParallelApplicativeTest {
  private[this] def timeOf(task: Task[Unit]) = {
    val start = System.currentTimeMillis()
    val _ = Await.ready(task.unsafeRunAsyncFuture, 1 minute)
    System.currentTimeMillis() - start
  }
  
  @Test def fs2ParallelApplicative(): Unit = {
    implicit val strategy = Strategy.fromCachedDaemonPool("ParallelApplicativeTest")
    
    val sleep = Parallel(Task(Thread.sleep(1000L)))
    val command = catsSyntaxTuple3Semigroupal[Parallel[Task, ?], Unit, Unit, Unit]((sleep, sleep, sleep)).mapN {(_, _, _) => }
    
    val timeSerial = timeOf(command.runSerialOrUnprincipled)
    assertTrue(timeSerial > 3000)
    assertFalse(timeSerial > 4000)
    
    val timeParallel = timeOf(command.runParallel)
    assertTrue(timeParallel > 1000)
    assertFalse(timeParallel > 2000)
  }
}