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

  private[this] def run(implicit mo: Monad[F], ap: Applicative[F]): F[A] =
    serial.cata[IdKK](
      foldMapF_[ParallelFF[IdKK, F, ?], F](
        foldMap_[Coproduct[F, F, ?], F](
          foldCP_[F, F, F](FunctionK.id, FunctionK.id)
        )(ap)
      )
    )(SerialF.functorKKSerialF)
        
  final def runSerialOrUnprincipled(implicit mo: Monad[F]): F[A] = run
  final def runSerialOrUnprincipled[G[_]](f: F ~> G)(implicit mo: Monad[G]): G[A] = compile(f).runSerialOrUnprincipled
  
  final def runParallel(implicit mo: Monad[F], pa: ParallelApplicative[F]): F[A] = run(mo, pa)
  final def runParallel[G[_]](f: F ~> G)(implicit mo: Monad[G], pa: ParallelApplicative[G]): G[A] =
    compile(f).runParallel
}