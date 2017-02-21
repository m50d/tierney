package tierney.free

import cats.~>
import cats.free.FreeApplicative
import tierney.core.FunctorK
import cats.Applicative

trait FreeApplicativeSupport {
  def compile_[F[_], G[_]](f: F ~> G): FreeApplicative[F, ?] ~> FreeApplicative[G, ?] = Lambda[FreeApplicative[F, ?] ~> FreeApplicative[G, ?]](_.compile(f))
  implicit val compile__ : FunctorK[FreeApplicative] = new FunctorK[FreeApplicative] {
    override def map[F[_], G[_]](f: F ~> G) = compile_(f)
  }
  def fold_[F[_]: Applicative]: FreeApplicative[F, ?] ~> F = Lambda[FreeApplicative[F, ?] ~> F](_.fold)
  def foldMap_[F[_], G[_]: Applicative](f: F ~> G): FreeApplicative[F, ?] ~> G = Lambda[FreeApplicative[F, ?] ~> G](_.foldMap(f))
  def lift_[F[_]]: F ~> FreeApplicative[F, ?] = Lambda[F ~> FreeApplicative[F, ?]](FreeApplicative.lift(_))
}
object FreeApplicativeSupport extends FreeApplicativeSupport