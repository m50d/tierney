package tierney.free

import cats.~>
import cats.free.Free
import tierney.core.FunctorK

trait FreeSupport {
  def compileF_[F[_], G[_]](f: F ~> G): Free[F, ?] ~> Free[G, ?] = Lambda[Free[F, ?] ~> Free[G, ?]](_.compile(f))
  implicit val compileF__ : FunctorK[Free] = new FunctorK[Free] {
    override def map[F[_], G[_]](f: F ~> G) = compileF_(f)
  }
}
object FreeSupport extends FreeSupport