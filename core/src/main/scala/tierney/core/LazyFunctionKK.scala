package tierney.core

class LazyFunctionKK[S[_[_], _], T[_[_], _]](value: => S ~~> T) extends (S ~~> T) {
  override def apply[F[_]] = value.apply[F]
}