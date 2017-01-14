package tierney.core

import cats.~>

trait FunctionKK[S[_[_], _], T[_[_], _]] extends Serializable { self =>
  def apply[F[_]]: S[F, ?] ~> T[F, ?]
}