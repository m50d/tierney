package tierney

import cats.Functor
import cats.~>
import cats.syntax.functor._

package object core {
  final implicit class Fix[F[_]](unfix: F[Fix[F]]) {
    def cata[A](f: F[A] ⇒ A)(implicit functor: Functor[F]): A =
      f(unfix map { _.cata(f) })
  }

  object Fix1 {
    def unfix1[F[_[_], _]]: Fix1[F, ?] ~> F[Fix1[F, ?], ?] =
      new (Fix1[F, ?] ~> F[Fix1[F, ?], ?]) {
        override def apply[A](fa: Fix1[F, A]) = fa.unfix
      }
    def cata1[F[_[_], _], G[_]](f: F[G, ?] ~> G)(implicit functor: Functor1[F]): Fix1[F, ?] ~> G =
      unfix1[F] andThen[F[G, ?]] functor.map[Fix1[F, ?], G](new Lazy[Fix1[F, ?], G](cata1(f))) andThen f
  }
  
  final implicit class Fix1[F[_[_], _], A](val unfix: F[Fix1[F, ?], A]) {
    def cata[G[_]](f: F[G, ?] ~> G)(implicit functor: Functor1[F]): G[A] = Fix1.cata1(f).apply(this) 
  }
}