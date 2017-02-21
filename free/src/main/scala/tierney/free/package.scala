package tierney

import cats.data.Coproduct
import cats.free.FreeApplicative
import cats.free.Free
import tierney.core._
import cats.~>
import tierney.free.FreeSupport

package object free extends CoproductSupport with FreeSupport with FreeApplicativeSupport {
  // TODO: It *might* be nicer to use a dedicated mutual recursion fixed point operator
  // to avoid creating garbage by wrapping and unwrapping to convert between serial and parallel trees
  // This would consume more memory though, and in any case be a performance optimization at most
  /** A fan of F commands and S constructs to execute parallelly
   */
  type ParallelFF[S[_[_], _], F[_], A] = FreeApplicative[Coproduct[F, S[F, ?], ?], A]
  object ParallelFF {
    implicit val functorKKParallelFF: FunctorKK[ParallelFF] = new FunctorKK[ParallelFF] {
      override def map[S[_[_], _], T[_[_], _]](f: S ~~> T) = new (Lambda[(F[_], A) ⇒ ParallelFF[S, F, A]] ~~> Lambda[(F[_], A) ⇒ ParallelFF[T, F, A]]) {
        override def apply[F[_]] = compile_[Coproduct[F, S[F, ?], ?], Coproduct[F, T[F, ?], ?]](rightMap[F, S[F, ?], T[F, ?]](f[F]))
      }
    }
  }
  /** A chain of S constructs to execute serially
   */
  type SerialFF[S[_[_], _], F[_], A] = Free[S[F, ?], A]
  object SerialFF {
    implicit val functorKKSerialFF: FunctorKK[SerialFF] = new FunctorKK[SerialFF] {
      override def map[S[_[_], _], T[_[_], _]](f: S ~~> T) = 
        new (Lambda[(F[_], A) ⇒ SerialFF[S, F, A]] ~~> Lambda[(F[_], A) ⇒ SerialFF[T, F, A]]) {
        override def apply[F[_]] = compileF_[S[F, ?], T[F, ?]](f[F])
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
  /** A fan of chains of parallel and serial F commands
   */
  type Parallel[F[_], A] = FixKK[ParallelF, F, A]
  def Parallel[F[_], A](command: F[A]): Parallel[F, A] =
    FixKK[ParallelF, F, A](FreeApplicative.lift[Coproduct[F, SerialFF[Parallel, F, ?], ?], A](Coproduct.leftc[F, SerialFF[Parallel, F, ?], A](command)))
  implicit class ParallelOps[F[_], A](override val parallel: Parallel[F, A]) extends AnyVal with TierneyFree[F, A] {
    override def serial: Serial[F, A] = parallel.cata[Serial](new (Lambda[(F[_], A) ⇒ ParallelF[Serial, F, A]] ~~> Serial) {
      override def apply[F[_]] =
        compile_[Coproduct[F, SerialFF[Serial, F, ?], ?], Coproduct[F, Serial[F, ?], ?]](
          rightMap[F, SerialFF[Serial, F, ?], Serial[F, ?]](
            compileF_[Serial[F, ?], ParallelFF[Serial, F, ?]](
              right_[F, Serial[F, ?]] andThen[ParallelFF[Serial, F, ?]] lift_[Coproduct[F, Serial[F, ?], ?]]
            ) andThen[Serial[F, ?]] fixKK[SerialF].apply[F]
          )
        ) andThen[Free[ParallelFF[Serial, F, ?], ?]] liftF_[ParallelFF[Serial, F, ?]] andThen[Serial[F, ?]] fixKK[SerialF].apply[F]
    })(ParallelF.functorKKParallelF)
  }
  /** A chain of fans of parallel and serial F commands
   */
  type Serial[F[_], A] = FixKK[SerialF, F, A]
  def Serial[F[_], A](command: F[A]): Serial[F, A] = Parallel(command).serial
  implicit class SerialOps[F[_], A](override val serial: Serial[F, A]) extends AnyVal with TierneyFree[F, A] {
    override def parallel: Parallel[F, A] = serial.cata[Parallel](new (Lambda[(F[_], A) ⇒ SerialF[Parallel, F, A]] ~~> Parallel) {
      override def apply[F[_]] =
        compileF_[ParallelFF[Parallel, F, ?], Parallel[F, ?]](
          compile_[Coproduct[F, Parallel[F, ?], ?], Coproduct[F, SerialFF[Parallel, F, ?], ?]](
            rightMap[F, Parallel[F, ?], SerialFF[Parallel, F, ?]](
              liftF_[Parallel[F, ?]]
            )
          ) andThen[Parallel[F, ?]] fixKK[ParallelF].apply[F]
        ) andThen[Coproduct[F, SerialFF[Parallel, F, ?], ?]]
        right_[F, SerialFF[Parallel, F, ?]] andThen[FreeApplicative[Coproduct[F, SerialFF[Parallel, F, ?], ?], ?]]
        lift_[Coproduct[F, SerialFF[Parallel, F, ?], ?]] andThen[Parallel[F, ?]] fixKK[ParallelF].apply[F]
    })(SerialF.functorKKSerialF)
  }
}