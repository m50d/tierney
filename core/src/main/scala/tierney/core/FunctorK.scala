package tierney.core

import cats.~>
import cats.Functor
import cats.free.Free
import cats.free.FreeApplicative
import cats.data.EitherK
import cats.arrow.FunctionK

/**
 * Higher-kinded functor
 */
trait FunctorK[S[_[_], _]] {
  self =>
  def map[F[_], G[_]](f: F ~> G): S[F, ?] ~> S[G, ?]
  final def andThen[T[_[_], _]](other: FunctorK[T]): FunctorK[Lambda[(F[_], A) => T[S[F, ?], A]]] =
    new FunctorK[Lambda[(F[_], A) => T[S[F, ?], A]]] {
    override def map[F[_], G[_]](f: F ~> G): T[S[F, ?], ?] ~> T[S[G, ?], ?] =
      other.map[S[F, ?], S[G, ?]](self.map(f))
  }
}
object FunctorK {
  implicit def freeFunctorK: FunctorK[Free] = new FunctorK[Free] {
     override def map[F[_], G[_]](f: F ~> G): Free[F, ?] ~> Free[G, ?] =
      Lambda[Free[F, ?] ~> Free[G, ?]](_.foldMap[Free[G, ?]](f andThen[Free[G, ?]] Lambda[G ~> Free[G, ?]](Free.liftF(_))))
  }
  implicit def freeApplicativeFunctorK: FunctorK[FreeApplicative] = new FunctorK[FreeApplicative] {
    override def map[F[_], G[_]](f: F ~> G): FreeApplicative[F, ?] ~> FreeApplicative[G, ?] = 
      Lambda[FreeApplicative[F, ?] ~> FreeApplicative[G, ?]](_.foldMap[FreeApplicative[G, ?]](f andThen[FreeApplicative[G, ?]] Lambda[G ~> FreeApplicative[G, ?]](FreeApplicative.lift(_))))
  }
  implicit def coproductFunctorK[F[_]]: FunctorK[Lambda[(G[_], A) => EitherK[F, G, A]]] = new FunctorK[Lambda[(G[_], A) => EitherK[F, G, A]]] {
    override def map[G[_], H[_]](f: G ~> H): EitherK[F, G, ?] ~> EitherK[F, H, ?] =
      Lambda[EitherK[F, G, ?] ~> EitherK[F, H, ?]](_.fold[EitherK[F, H, ?]](Lambda[F ~> EitherK[F, H, ?]](EitherK.left(_)), f andThen[EitherK[F, H, ?]] Lambda[H ~> EitherK[F, H, ?]](EitherK.right(_))))
  }
}