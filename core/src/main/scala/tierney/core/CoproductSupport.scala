package tierney.core

import cats.~>
import cats.data.Coproduct

/**
 * Useful Coproduct-related functions
 */
trait CoproductSupport {
  def left_[F[_], G[_]]: F ~> Coproduct[F, G, ?] = Lambda[F ~> Coproduct[F, G, ?]](Coproduct.leftc(_))
  def right_[F[_], G[_]]: G ~> Coproduct[F, G, ?] = Lambda[G ~> Coproduct[F, G, ?]](Coproduct.rightc(_))
  def rightMap[F[_], G[_], H[_]](f: G ~> H): Coproduct[F, G, ?] ~> Coproduct[F, H, ?] =
    Lambda[Coproduct[F, G, ?] ~> Coproduct[F, H, ?]](_.fold[Coproduct[F, H, ?]](left_[F, H], f andThen[Coproduct[F, H, ?]] right_[F, H]))
}
object CoproductSupport extends CoproductSupport