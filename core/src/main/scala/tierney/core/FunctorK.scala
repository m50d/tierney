package tierney.core

import cats.~>
import cats.Functor

/**
 * Higher-kinded functor
 */
trait FunctorK[S[_[_], _]] {
  def map[F[_], G[_]](f: F ~> G): S[F, ?] ~> S[G, ?]
}