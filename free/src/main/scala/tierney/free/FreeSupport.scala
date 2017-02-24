package tierney.free

import cats.~>
import cats.free.Free
import tierney.core.FunctorK
import cats.Monad

trait FreeSupport {
  def compileF_[F[_], G[_]](f: F ~> G): Free[F, ?] ~> Free[G, ?] = Lambda[Free[F, ?] ~> Free[G, ?]](_.compile(f))
  implicit val compileF__ : FunctorK[Free] = new FunctorK[Free] {
    override def map[F[_], G[_]](f: F ~> G) = compileF_(f)
  }
  def foldMapF_[F[_], G[_]: Monad](f: F ~> G): Free[F, ?] ~> G = Lambda[Free[F, ?] ~> G](_.foldMap(f))
  def liftF_[F[_]]: F ~> Free[F, ?] = Lambda[F ~> Free[F, ?]](Free.liftF(_))
}
object FreeSupport extends FreeSupport