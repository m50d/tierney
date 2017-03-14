package tierney.parallel

import cats.Applicative

/** Marker trait for Applicative instances which run applies in parallel
 */
trait ParallelApplicative[F[_]] extends Applicative[F]