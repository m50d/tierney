package tierney.free

trait TierneyFree[F[_], A] extends Any {
  def parallel: Parallel[F, A]
  def serial: Serial[F, A]
}