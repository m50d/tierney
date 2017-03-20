package tierney.core

import cats.~>

trait FunctorKK[W[_[_[_], _], _[_], _]] {
  self =>
  //The ? syntax for kind-projector ought to support this use case, but it doesn't seem to
  def map[S[_[_], _], T[_[_], _], F[_]](f: S[F, ?] ~> T[F, ?]): W[S, F, ?] ~> W[T, F, ?]
  
  final def andThen[V[_[_[_], _], _[_], _]](other: FunctorKK[V]): FunctorKK[Lambda[(S[_[_], _], F[_], A) => V[Lambda[(G[_], B) => W[S, G, B]], F, A]]] =
    new FunctorKK[Lambda[(S[_[_], _], F[_], A) => V[Lambda[(G[_], B) => W[S, G, B]], F, A]]] {
    override def map[S[_[_], _], T[_[_], _], F[_]](f: S[F, ?] ~> T[F, ?]) = other.map[Lambda[(G[_], A) => W[S, G, A]], Lambda[(G[_], A) => W[T, G, A]], F](self.map(f))
  }
}