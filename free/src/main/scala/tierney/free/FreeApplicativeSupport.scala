package tierney.free

import cats.~>
import cats.free.FreeApplicative
import tierney.core.FunctorK

trait FreeApplicativeSupport {
  def compile_[F[_], G[_]](f: F ~> G): FreeApplicative[F, ?] ~> FreeApplicative[G, ?] = Lambda[FreeApplicative[F, ?] ~> FreeApplicative[G, ?]](_.compile(f))
  implicit val compile__ : FunctorK[FreeApplicative] = new FunctorK[FreeApplicative] {
    override def map[F[_], G[_]](f: F ~> G) = compile_(f)
  }
  def lift_[F[_]]: F ~> FreeApplicative[F, ?] = Lambda[F ~> FreeApplicative[F, ?]](FreeApplicative.lift(_))
}
object FreeApplicativeSupport extends FreeApplicativeSupport