package tierney.core

import cats.~>

/**
 * Higher-kinded functor
 */
trait FunctorK[F[_[_], _]] {
  def map[G[_], H[_]](f: G ~> H): F[G, ?] ~> F[H, ?]
}