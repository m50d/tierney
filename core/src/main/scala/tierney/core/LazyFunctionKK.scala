package tierney.core

import cats.~>

class LazyFunctionKK[S[_[_], _], T[_[_], _], F[_]](value: => S[F, ?] ~> T[F, ?]) extends (S[F, ?] ~> T[F, ?]) {
  override def apply[A](sfa: S[F, A]) = value(sfa)
}