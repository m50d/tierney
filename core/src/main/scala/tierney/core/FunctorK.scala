package tierney.core

import cats.~>
import cats.Functor
import cats.free.Free
import cats.free.FreeApplicative
import cats.data.Coproduct
import cats.arrow.FunctionK

/**
 * Higher-kinded functor
 */
trait FunctorK[S[_[_], _]] {
  def map[F[_], G[_]](f: F ~> G): S[F, ?] ~> S[G, ?]
}
object FunctorK {
  implicit def freeFunctorK: FunctorK[Free] = new FunctorK[Free] {
     override def map[F[_], G[_]](f: F ~> G): Free[F, ?] ~> Free[G, ?] = new (Free[F, ?] ~> Free[G, ?]){
       override def apply[A](fa: Free[F, A]) =
         fa.foldMap[Free[G, ?]](f andThen[Free[G, ?]] Lambda[G ~> Free[G, ?]](Free.liftF(_)))
     }
  }
  implicit def freeApplicativeFunctorK: FunctorK[FreeApplicative] = new FunctorK[FreeApplicative] {
    override def map[F[_], G[_]](f: F ~> G): FreeApplicative[F, ?] ~> FreeApplicative[G, ?] = new (FreeApplicative[F, ?] ~> FreeApplicative[G, ?]){
       override def apply[A](fa: FreeApplicative[F, A]) =
         fa.foldMap[FreeApplicative[G, ?]](f andThen[FreeApplicative[G, ?]] Lambda[G ~> FreeApplicative[G, ?]](FreeApplicative.lift(_)))
     }
  }
  implicit def coproductFunctorK[F[_]]: FunctorK[Lambda[(G[_], A) => Coproduct[F, G, A]]] = new FunctorK[Lambda[(G[_], A) => Coproduct[F, G, A]]] {
    override def map[G[_], H[_]](f: G ~> H): Coproduct[F, G, ?] ~> Coproduct[F, H, ?] =
      new (Coproduct[F, G, ?] ~> Coproduct[F, H, ?]) {
      override def apply[A](fa: Coproduct[F, G, A]) = fa.fold[Coproduct[F, H, ?]](Lambda[F ~> Coproduct[F, H, ?]](Coproduct.left(_)), f andThen[Coproduct[F, H, ?]] Lambda[H ~> Coproduct[F, H, ?]](Coproduct.right(_)))
    }
  }
}