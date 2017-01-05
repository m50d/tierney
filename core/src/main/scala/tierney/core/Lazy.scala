package tierney.core

import cats.~>

private[core] class Lazy[F[_], G[_]](value: => F ~> G) extends (F ~> G) {
  override def apply[A](fa: F[A]) = value(fa)
}