package tierney

import cats.data.Coproduct
import cats.free.FreeApplicative
import cats.free.Free
import tierney.core._
import cats.~>

package object free {
  // TODO: It *might* be nicer to use a dedicated mutual recursion fixed point operator
  // to avoid creating garbage by wrapping and unwrapping to convert between serial and parallel trees
  // This would consume more memory though, and in any case be a performance optimization at most
  /** A fan of F commands and S constructs to execute parallelly
   */
  type ParallelFF[S[_[_], _], F[_], A] = FreeApplicative[Coproduct[F, S[F, ?], ?], A]
  object ParallelFF {
    implicit val functorKKParallelFF: FunctorKK[ParallelFF] = new FunctorKK[ParallelFF] {
      override def map[S[_[_], _], T[_[_], _]](f: S ~~> T) = new (Lambda[(F[_], A) ⇒ ParallelFF[S, F, A]] ~~> Lambda[(F[_], A) ⇒ ParallelFF[T, F, A]]) {
        override def apply[F[_]] = new (ParallelFF[S, F, ?] ~> ParallelFF[T, F, ?]) {
          override def apply[B](fb: FreeApplicative[Coproduct[F, S[F, ?], ?], B]) =
            fb.compile[Coproduct[F, T[F, ?], ?]](Lambda[Coproduct[F, S[F, ?], ?] ~> Coproduct[F, T[F, ?], ?]](
              _.fold[Coproduct[F, T[F, ?], ?]](new (F ~> Coproduct[F, T[F, ?], ?]) {
                override def apply[D](fd: F[D]) = Coproduct.leftc[F, T[F, ?], D](fd)
              }, new (S[F, ?] ~> Coproduct[F, T[F, ?], ?]) {
                override def apply[D](fd: S[F, D]) = Coproduct.rightc[F, T[F, ?], D](f[F](fd))
              })))
        }
      }
    }
  }
  /** A chain of S constructs to execute serially
   */
  type SerialFF[S[_[_], _], F[_], A] = Free[S[F, ?], A]
  object SerialFF {
    implicit val functorKKSerialFF: FunctorKK[SerialFF] = new FunctorKK[SerialFF] {
      override def map[S[_[_], _], T[_[_], _]](f: S ~~> T) = new (Lambda[(F[_], A) ⇒ SerialFF[S, F, A]] ~~> Lambda[(F[_], A) ⇒ SerialFF[T, F, A]]) {
        override def apply[F[_]] = Lambda[SerialFF[S, F, ?] ~> SerialFF[T, F, ?]](_.compile[T[F, ?]](f[F]))
      }
    }
  }
  /** A fan of chains of S constructs to execute parallelly
   */
  type ParallelF[S[_[_], _], F[_], A] = ParallelFF[Lambda[(F[_], A) ⇒ SerialFF[S, F, A]], F, A]
  object ParallelF {
    implicit val functorKKParallelF: FunctorKK[ParallelF] = new FunctorKK[ParallelF] {
      override def map[S[_[_], _], T[_[_], _]](f: S ~~> T) =
        ParallelFF.functorKKParallelFF.map[Lambda[(F[_], A) ⇒ SerialFF[S, F, A]], Lambda[(F[_], A) ⇒ SerialFF[T, F, A]]](SerialFF.functorKKSerialFF.map(f))
    }
  }
  /** A chain of fans of F commands and S constructs to execute serially
   */
  type SerialF[S[_[_], _], F[_], A] = SerialFF[Lambda[(F[_], A) ⇒ ParallelFF[S, F, A]], F, A]
  object SerialF {
    implicit val functorKKSerialF: FunctorKK[SerialF] = new FunctorKK[SerialF] {
      override def map[S[_[_], _], T[_[_], _]](f: S ~~> T) =
        SerialFF.functorKKSerialFF.map[Lambda[(F[_], A) ⇒ ParallelFF[S, F, A]], Lambda[(F[_], A) ⇒ ParallelFF[T, F, A]]](ParallelFF.functorKKParallelFF.map(f))
    }
  }
  /**
   * A fan of chains of parallel and serial F commands
   */
  type Parallel[F[_], A] = FixKK[ParallelF, F, A]
  def Parallel[F[_], A](command: F[A]): Parallel[F, A] = 
    FixKK[ParallelF, F, A](FreeApplicative.lift[Coproduct[F, SerialFF[Parallel, F, ?], ?], A](Coproduct.leftc[F, SerialFF[Parallel, F, ?], A](command)))
//  implicit class ParallelOps[F[_], A](val value: Parallel[F, A]) extends AnyVal {
//    def serial: Serial[F, A] = value.cata[Serial](new (Lambda[(F[_], A) ⇒ ParallelF[Serial, F, A]] ~~> Serial){
//      override def apply[F[_]] = new (ParallelF[Serial, F, ?] ~> Serial[F, ?]) {
//        override def apply[A](fa: ParallelF[Serial, F, A]) =
//          FixKK[SerialF, F, A](Free.liftF[ParallelFF[Lambda[(F[_], A) ⇒ SerialFF[Serial, F, A]], F, ?], A](fa))
//      }
//    })(ParallelF.functorKKParallelF)
//  }
  /**
   * A chain of fans of parallel and serial F commands
   */
  type Serial[F[_], A] = FixKK[SerialF, F, A]
//  def Serial[F[_], A](command: F[A]): Serial[F, A] =
}