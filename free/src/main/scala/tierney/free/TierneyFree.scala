package tierney.free

import cats.free.Free
import cats.Applicative
import tierney.core._
import CoproductSupport._
import FreeApplicativeSupport._
import FreeSupport._
import cats.data.Coproduct

trait TierneyFree[F[_], A] extends Any {
  def parallel: Parallel[F, A]
  def serial: Serial[F, A]
  
  final def runParallelFans(implicit ap: Applicative[F]): Free[F, A] =
    serial.cata[Free](new (Lambda[(F[_], A) => SerialF[Free, F, A]] ~~> Free){
      override def apply[F[_]] =
        compileF_[ParallelFF[Free, F, ?], F](
          null
//          foldMap_[Coproduct[F, Free[F, ?], ?], F](null)(ap)
        )
    })(SerialF.functorKKSerialF)
}