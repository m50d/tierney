package tierney

import cats.Functor
import cats.syntax.functor._

final case class Fix[F[_]](unfix: F[Fix[F]]) {
  def cata[A](f: F[A] => A)(implicit functor: Functor[F]): A =
    f(unfix map {_.cata(f)})
}