package tierney

import cats.Functor
import cats.~>
import cats.syntax.functor._

package object core {
  type ~~>[S[_[_], _], T[_[_], _]] = FunctionKK[S, T]
  
  final implicit class Fix[F[_]](val unfix: F[Fix[F]]) {
    def cata[A](f: F[A] ⇒ A)(implicit functor: Functor[F]): A =
      f(unfix map { _.cata(f) })
  }

  object FixK {
    private[this] def unfixK[S[_[_], _]] = Lambda[FixK[S, ?] ~> S[FixK[S, ?], ?]](_.unfix)
    def cataK[S[_[_], _], F[_]](f: S[F, ?] ~> F)(implicit functor: FunctorK[S]): FixK[S, ?] ~> F =
      unfixK[S] andThen[S[F, ?]] functor.map[FixK[S, ?], F](new Lazy[FixK[S, ?], F](cataK(f))) andThen f
  }
  final implicit class FixK[F[_[_], _], A](val unfix: F[FixK[F, ?], A]) {
    def cata[G[_]](f: F[G, ?] ~> G)(implicit functor: FunctorK[F]): G[A] = FixK.cataK(f).apply(this) 
  }
  
  final implicit class FixKK[W[_[_[_], _], _[_], _], F[_], A](val unfix: W[Lambda[(F[_], A) => FixKK[W, F, A]], F, A]) {
//    def cata[S[_[_], _]](f: )
  }
}

package core {
object FixKK {
    private[this] def unfixKK[W[_[_[_], _], _[_], _]] =
      new (Lambda[(F[_], A) => FixKK[W, F, A]] ~~> Lambda[(F[_], A) => W[Lambda[(G[_], B) => FixKK[W, G, B]], F, A]]){
        override def apply[F[_]] = Lambda[FixKK[W, F, ?] ~> W[Lambda[(F[_], A) => FixKK[W, F, A]], F, ?]](_.unfix)
    }
    def cataKK[W[_[_[_], _], _[_], _], S[_[_], _]](f:Lambda[(F[_], A) => W[S, F, A]] ~~> S)(implicit functor: FunctorKK[W]): Lambda[(F[_], A) => FixKK[W, F, A]] ~~> S =
      null
  }
}