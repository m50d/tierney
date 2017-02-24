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

trait TierneyFree[F[_], A] extends Any {
  def parallel: Parallel[F, A]
  def serial: Serial[F, A]
  
  final def runParallelFans(implicit ap: Applicative[F]): Free[F, A] =
    serial.cata[Free](foldMapF_[ParallelFF[Free, F, ?], Free[F, ?]](
        foldMap_[Coproduct[F, Free[F, ?], ?], Free[F, ?]](
          foldCP_[F, Free[F, ?], Free[F, ?]](liftF_, FunctionK.id[Free[F, ?]])
        )
      )
    )(SerialF.functorKKSerialF)
}