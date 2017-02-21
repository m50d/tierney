package tierney.free

import cats.~>
import cats.free.FreeApplicative

trait FreeApplicativeSupport {
  def lift_[F[_]]: F ~> FreeApplicative[F, ?] = Lambda[F ~> FreeApplicative[F, ?]](FreeApplicative.lift(_))
  def compile_[F[_], G[_]](f: F ~> G): FreeApplicative[F, ?] ~> FreeApplicative[G, ?] = Lambda[FreeApplicative[F, ?] ~> FreeApplicative[G, ?]](_.compile(f))
}
object FreeApplicativeSupport extends FreeApplicativeSupport