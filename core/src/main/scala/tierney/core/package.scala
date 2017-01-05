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
    def unfix1[F[_[_], _]]: Partial2[Fix1, F]#O ~> Partial1[F, Partial2[Fix1, F]#O]#O =
      new (Partial2[Fix1, F]#O ~> Partial1[F, Partial2[Fix1, F]#O]#O) {
        override def apply[A](fa: Fix1[F, A]) = fa.unfix
      }
    def cata1[F[_[_], _], G[_]](f: Partial1[F, G]#O ~> G)(implicit functor: Functor1[F]): Partial2[Fix1, F]#O ~> G =
      unfix1[F] andThen[Partial1[F, G]#O] functor.map[Partial2[Fix1, F]#O, G](new Lazy[Partial2[Fix1, F]#O, G](cata1(f))) andThen f
  }
  
  final implicit class Fix1[F[_[_], _], A](val unfix: F[Partial2[Fix1, F]#O, A]) {
    def cata[G[_]](f: Partial1[F, G]#O ~> G)(implicit functor: Functor1[F]): G[A] = Fix1.cata1(f).apply(this) 
  }
}