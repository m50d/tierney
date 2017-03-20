package tierney

import cats.data.Coproduct
import cats.free.FreeApplicative
import cats.free.Free
import tierney.core._
import cats.~>
import tierney.free.FreeSupport
import cats.Applicative
import cats.Monad

package object free extends CoproductSupport with FreeSupport with FreeApplicativeSupport {
  /** Either an immediate command F or a recursive S structure
   */
  type Node[S[_[_], _], F[_], A] = Coproduct[F, S[F, ?], A]
  object Node {
    implicit val functorKKNode: FunctorKK[Node] = new FunctorKK[Node] {
      override def map[S[_[_], _], T[_[_], _], F[_]](f: S[F, ?] ~> T[F, ?]) =
        rightMap[F, S[F, ?], T[F, ?]](f)
    }
    def right[S[_[_], _], F[_], A](s: S[F, A]): Node[S, F, A] =
      Coproduct.rightc[F, S[F, ?], A](s)
  }
  
  /** A fan of F commands and S constructs to execute parallelly
   */
  type ParallelFF[S[_[_], _], F[_], A] = FreeApplicative[Node[S, F, ?], A]
  object ParallelFF {
    implicit val functorKKParallelFF: FunctorKK[ParallelFF] = new FunctorKK[ParallelFF] {
      override def map[S[_[_], _], T[_[_], _], F[_]](f: S[F, ?] ~> T[F, ?]) = 
        compile_[Coproduct[F, S[F, ?], ?], Coproduct[F, T[F, ?], ?]](rightMap[F, S[F, ?], T[F, ?]](f))
    }
  }
  
  /** A chain of S constructs to execute serially
   */
  type SerialFF[S[_[_], _], F[_], A] = Free[S[F, ?], A]
  object SerialFF {
    implicit val functorKKSerialFF: FunctorKK[SerialFF] = new FunctorKK[SerialFF] {
      override def map[S[_[_], _], T[_[_], _], F[_]](f: S[F, ?] ~> T[F, ?]) =
        compileF_[S[F, ?], T[F, ?]](f)
    }
  }
  /** A fan of chains of S constructs to execute parallelly
   */
  type ParallelF[S[_[_], _], F[_], A] = ParallelFF[Lambda[(G[_], B) ⇒ SerialFF[S, G, B]], F, A]
  object ParallelF {
    implicit val functorKKParallelF: FunctorKK[ParallelF] = SerialFF.functorKKSerialFF andThen ParallelFF.functorKKParallelFF
  }
  /** A fan of chains of parallel and serial F commands
   */
  type Parallel[F[_], A] = FixKK[ParallelF, F, A]
  object Parallel {
    def apply[F[_], A](command: F[A]): Parallel[F, A] =
      FixKK[ParallelF, F, A](FreeApplicative.lift[Coproduct[F, SerialFF[Parallel, F, ?], ?], A](Coproduct.leftc[F, SerialFF[Parallel, F, ?], A](command)))
  }
  implicit def applicativeParallel[F[_]]: Applicative[Parallel[F, ?]] = new Applicative[Parallel[F, ?]] {
    override def pure[A](a: A) = FixKK[ParallelF, F, A](FreeApplicative.pure[Coproduct[F, SerialFF[Parallel, F, ?], ?], A](a))
    override def ap[A, B](ff: Parallel[F, A => B])(fa: Parallel[F, A]) =
      FixKK[ParallelF, F, B](fa.unfix.ap(ff.unfix))
  }
  implicit def functorKParallel: FunctorK[Parallel] = new FunctorK[Parallel] {
    override def map[F[_], G[_]](f: F ~> G) =
      unfixKK[ParallelF, F] andThen[ParallelF[Parallel, G, ?]] compile_[Coproduct[F, SerialFF[Parallel, F, ?], ?], Coproduct[G, SerialFF[Parallel, G, ?], ?]](
        foldCP_[F, SerialFF[Parallel, F, ?], Coproduct[G, SerialFF[Parallel, G, ?], ?]](
          f andThen[Coproduct[G, SerialFF[Parallel, G, ?], ?]] left_[G, SerialFF[Parallel, G, ?]],
          compileF_[Parallel[F, ?], Parallel[G, ?]](
            new LazyFunctionK[Parallel[F, ?], Parallel[G, ?]](map(f))
          ) andThen[Coproduct[G, SerialFF[Parallel, G, ?], ?]] right_[G, SerialFF[Parallel, G, ?]])
      ) andThen[Parallel[G, ?]] fixKK[ParallelF, G]
  }
  final implicit class ParallelOps[F[_], A](override val parallel: Parallel[F, A]) extends AnyVal with TierneyFree[F, A] {
    override def serial: Serial[F, A] = Free.liftF[Parallel[F, ?], A](parallel)
    def localCompile[G[_]](f: F ~> G): Parallel[G, A] = functorKParallel.map(f).apply(parallel)
  }
  /** A chain of fans of parallel and serial F commands
   */
  type Serial[F[_], A] = SerialFF[Parallel, F, A]
  object Serial {
    def apply[F[_], A](command: F[A]): Serial[F, A] = Parallel(command).serial
  }
  implicit def monadSerial[F[_]]: Monad[Serial[F, ?]] = new Monad[Serial[F, ?]] {
    override def pure[A](a: A) = Free.pure[Parallel[F, ?], A](a)
    override def flatMap[A, B](fa: Serial[F, A])(f: A => Serial[F, B]) =
      fa.flatMap(f)
    override def tailRecM[A, B](a: A)(f: A => Serial[F, Either[A, B]]) =
       // recursion is OK as Free is lazy 
      flatMap(f(a))(_.fold(tailRecM(_)(f), pure))
  }
  implicit def functorKSerial: FunctorK[Serial] = new FunctorK[Serial] {
    override def map[F[_], G[_]](f: F ~> G) =
      compileF_[Parallel[F, ?], Parallel[G, ?]](
        functorKParallel.map(f)
      )
  }
  final implicit class SerialOps[F[_], A](override val serial: Serial[F, A]) extends AnyVal with TierneyFree[F, A] {
    override def parallel: Parallel[F, A] =
      new FixKK[ParallelF, F, A](FreeApplicative.lift[Node[Serial, F, ?], A](Node.right(serial)))
    def localCompile[G[_]](f: F ~> G): Serial[G, A] = functorKSerial.map(f).apply(serial)
  }
}