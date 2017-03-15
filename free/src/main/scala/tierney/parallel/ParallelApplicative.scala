package tierney.parallel

import cats.Applicative
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import monix.async.{ Task ⇒ MonixTask }
import fs2.{ Task ⇒ Fs2Task }
import fs2.Strategy

/** Marker trait for Applicative instances which run applies in parallel
 */
trait ParallelApplicative[F[_]] extends Applicative[F]

object ParallelApplicative {
  implicit def parallelApplicativeFuture(implicit ec: ExecutionContext): ParallelApplicative[Future] =
    new ParallelApplicative[Future] {
      override def pure[A](a: A) = Future.successful(a)
      /** Scala Future is unprincipled
       */
      override def ap[A, B](ff: Future[A ⇒ B])(fa: Future[A]) = for {
        f ← ff
        a ← fa
      } yield f(a)
    }
  implicit def parallelApplicativeMonixTask: ParallelApplicative[MonixTask] = new ParallelApplicative[MonixTask] {
    override def pure[A](a: A) = MonixTask.now(a)
    override def ap[A, B](ff: MonixTask[A ⇒ B])(fa: MonixTask[A]) =
      MonixTask.map2(ff, fa)(_(_))
  }
  implicit def parallelApplicativeFs2Task(implicit s: Strategy): ParallelApplicative[Fs2Task] = new ParallelApplicative[Fs2Task] {
    override def pure[A](a: A) = Fs2Task.now(a)
    override def ap[A, B](ff: Fs2Task[A ⇒ B])(fa: Fs2Task[A]) = for {
      sf ← Fs2Task.start(ff)
      sa ← Fs2Task.start(fa)
      f ← sf
      a ← sa
    } yield f(a)
  }
}