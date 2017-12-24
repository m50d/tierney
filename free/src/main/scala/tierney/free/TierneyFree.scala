package tierney.free

import cats.free.Free
import cats.Applicative
import tierney.core._
import EitherKSupport._
import FreeApplicativeSupport._
import FreeSupport._
import cats.data.EitherK
import cats.~>
import cats.arrow.FunctionK
import cats.Monad
import tierney.parallel.ParallelApplicative
import cats.free.FreeApplicative

/** Children must override at least one of node/parallel/serial
 */
trait TierneyFree[F[_], A] extends Any {
  def node: Node[F, A] = Node.right(serial)
  def parallel: Parallel[F, A] = FreeApplicative.lift[Node[F, ?], A](node)
  def serial: Serial[F, A] = Free.liftF[Parallel[F, ?], A](parallel)

  final def compile[G[_]](f: F ~> G): Node[G, A] = functorKNode.map(f).apply(node)

  private[this] def run(implicit mo: Monad[F], ap: Applicative[F]): F[A] =
    node.cata[IdKK](
      foldCP_[F, Free[FreeApplicative[F, ?], ?], F](
        FunctionK.id,
        foldMapF_[FreeApplicative[F, ?], F](fold_(ap)))
    )(NodeSerialParallelF.functorKKNodeSerialParallelF)

  final def runSerialOrUnprincipled(implicit mo: Monad[F]): F[A] = run
  final def runSerialOrUnprincipled[G[_]](f: F ~> G)(implicit mo: Monad[G]): G[A] = compile(f).runSerialOrUnprincipled

  final def runParallel(implicit mo: Monad[F], pa: ParallelApplicative[F]): F[A] = run(mo, pa)
  final def runParallel[G[_]](f: F ~> G)(implicit mo: Monad[G], pa: ParallelApplicative[G]): G[A] = compile(f).runParallel
}