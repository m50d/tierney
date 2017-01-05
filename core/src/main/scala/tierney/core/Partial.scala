package tierney.core

/**
 * Type-level helper function for partially applying F to G
 */
sealed trait Partial1[F[_[_], _], G[_]] {
  final type O[A] = F[G, A]
}

/**
 * Type-level helper function for partially applying F to G
 */
sealed trait Partial2[F[_[_[_], _], _], G[_[_], _]] { 
  final type O[A] = F[G, A]
}