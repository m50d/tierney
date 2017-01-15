package tierney.core

trait FunctorKK[W[_[_[_], _], _[_], _]] {
  //TODO the ? syntax for kind-projector ought to support this use case
  def map[S[_[_], _], T[_[_], _]](f: S ~~> T): Lambda[(F[_], A) => W[S, F, A]] ~~> Lambda[(F[_], A) => W[T, F, A]]
}