package tierney

case class Fix[F[_]](unfix: F[Fix[F]])