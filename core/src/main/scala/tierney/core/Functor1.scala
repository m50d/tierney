package tierney.core

import cats.~>

/**
 * Higher-kinded functor
 */
trait Functor1[F[_[_], _]] {
  def map[G[_], H[_]](f: G ~> H): F[G, ?] ~> F[H, ?]
}