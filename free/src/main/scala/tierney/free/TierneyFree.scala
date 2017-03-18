package tierney.free

import cats.free.Free
import cats.Applicative
import tierney.core._
import CoproductSupport._
import FreeApplicativeSupport._
import FreeSupport._
import cats.data.Coproduct
import cats.~>
import cats.arrow.FunctionK
import cats.Monad
import tierney.parallel.ParallelApplicative
import cats.free.FreeApplicative

trait TierneyFree[F[_], A] extends Any {
  def parallel: Parallel[F, A]
  def serial: Serial[F, A]
  final def compile[G[_]](f: F ~> G): Serial[G, A] = serial.localCompile(f)

  private[this] def flattenParallelFans(implicit ap: Applicative[F]): Free[F, A] =
    serial.cata[Free](foldMapF_[ParallelFF[Free, F, ?], Free[F, ?]](
      foldMap_[Coproduct[F, Free[F, ?], ?], Free[F, ?]](
        foldCP_[F, Free[F, ?], Free[F, ?]](liftF_, FunctionK.id[Free[F, ?]]))))(SerialF.functorKKSerialF)
  
//  private[this] def flattenSerialChains(implicit mo: Monad[F]): SemiParallel[F, A] =
//    parallel.cata[SemiParallel](
//      foldMap_[Coproduct[F, Free[SemiParallel[F, ?], ?], ?], SemiParallel[F, ?]](
//        foldCP_[F, Free[SemiParallel[F, ?], ?], SemiParallel[F, ?]](
//          left_[F, SemiParallel[F, ?]] andThen[ParallelFF[SemiParallel, F, ?]] 
//            lift_[Coproduct[F, SemiParallel[F, ?], ?]] andThen[SemiParallel[F, ?]] fixKK[ParallelFF, F],
//          null)
//      )
//    )(ParallelF.functorKKParallelF)
        
  final def runSerialOrUnprincipled(implicit mo: Monad[F]): F[A] =
    flattenParallelFans.runTailRec
  final def runSerialOrUnprincipled[G[_]](f: F ~> G)(implicit mo: Monad[G]): G[A] = compile(f).runSerialOrUnprincipled
  
  final def runParallel(implicit mo: Monad[F], pa: ParallelApplicative[F]): F[A] =
    flattenParallelFans(pa).runTailRec
  final def runParallel[G[_]](f: F ~> G)(implicit mo: Monad[G], pa: ParallelApplicative[G]): G[A] =
    compile(f).runParallel
}