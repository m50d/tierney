package tierney.core

import cats.~>
import cats.data.EitherK

/**
 * Useful EitherK-related functions
 */
trait EitherKSupport {
  def foldCP_[F[_], G[_], H[_]](f: F ~> H, g: G ~> H): EitherK[F, G, ?] ~> H = Lambda[EitherK[F, G, ?] ~> H](_.fold(f, g))
  def left_[F[_], G[_]]: F ~> EitherK[F, G, ?] = Lambda[F ~> EitherK[F, G, ?]](EitherK.leftc(_))
  def right_[F[_], G[_]]: G ~> EitherK[F, G, ?] = Lambda[G ~> EitherK[F, G, ?]](EitherK.rightc(_))
  def rightMap[F[_], G[_], H[_]](f: G ~> H): EitherK[F, G, ?] ~> EitherK[F, H, ?] =
    Lambda[EitherK[F, G, ?] ~> EitherK[F, H, ?]](_.fold[EitherK[F, H, ?]](left_[F, H], f andThen[EitherK[F, H, ?]] right_[F, H]))
  def rightMap_[F[_]] : FunctorK[Lambda[(G[_], A) => EitherK[F, G, A]]] = new FunctorK[Lambda[(G[_], A) => EitherK[F, G, A]]] {
    override def map[G[_], H[_]](f: G ~> H) = rightMap(f)
  }
}
object EitherKSupport extends EitherKSupport