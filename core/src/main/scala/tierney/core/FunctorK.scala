package tierney.core

import cats.~>
import cats.Functor
import cats.free.Free

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
}