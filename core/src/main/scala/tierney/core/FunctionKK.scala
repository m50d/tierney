package tierney.core

import cats.~>

trait FunctionKK[S[_[_], _], T[_[_], _]] extends Serializable { self =>
  def apply[F[_]]: S[F, ?] ~> T[F, ?]
  final def andThen[U[_[_], _]](other: T ~~> U): S ~~> U = new (S ~~> U) {
    override def apply[F[_]] = self.apply[F] andThen[U[F, ?]] other.apply[F]
  }
}