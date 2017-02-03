package tierney

import cats.Functor
import cats.~>
import cats.syntax.functor._

package object core {
  type ~~>[S[_[_], _], T[_[_], _]] = FunctionKK[S, T]
  
  final implicit class Fix[F[_]](unfix: F[Fix[F]]) {
    def cata[A](f: F[A] ⇒ A)(implicit functor: Functor[F]): A =
      f(unfix map { _.cata(f) })
  }

  object FixK {
    private[this] def unfixK[F[_[_], _]] = Lambda[FixK[F, ?] ~> F[FixK[F, ?], ?]](_.unfix)
    def cataK[F[_[_], _], G[_]](f: F[G, ?] ~> G)(implicit functor: FunctorK[F]): FixK[F, ?] ~> G =
      unfixK[F] andThen[F[G, ?]] functor.map[FixK[F, ?], G](new Lazy[FixK[F, ?], G](cataK(f))) andThen f
  }
  
  final implicit class FixK[F[_[_], _], A](val unfix: F[FixK[F, ?], A]) {
    def cata[G[_]](f: F[G, ?] ~> G)(implicit functor: FunctorK[F]): G[A] = FixK.cataK(f).apply(this) 
  }
}