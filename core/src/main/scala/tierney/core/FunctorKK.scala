package tierney.core

trait FunctorKK[W[_[_[_], _], _[_], _]] {
  self =>
  //TODO the ? syntax for kind-projector ought to support this use case
  def map[S[_[_], _], T[_[_], _]](f: S ~~> T): Lambda[(F[_], A) => W[S, F, A]] ~~> Lambda[(F[_], A) => W[T, F, A]]
  
  final def andThen[V[_[_[_], _], _[_], _]](other: FunctorKK[V]): FunctorKK[Lambda[(S[_[_], _], F[_], A) => V[Lambda[(G[_], B) => W[S, G, B]], F, A]]] =
    new FunctorKK[Lambda[(S[_[_], _], F[_], A) => V[Lambda[(G[_], B) => W[S, G, B]], F, A]]] {
    override def map[S[_[_], _], T[_[_], _]](f: S ~~> T) = other.map[Lambda[(F[_], A) => W[S, F, A]], Lambda[(F[_], A) => W[T, F, A]]](self.map(f))
  }
}