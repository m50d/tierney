package tierney.free

import cats.free.Free
import cats.Applicative
import tierney.core._
import cats.~>
import cats.arrow.FunctionK
import cats.Monad
import tierney.parallel.ParallelApplicative
import cats.free._
import cats.kernel.Monoid

/**
 * Children must override at least one of node/parallel/serial
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
        foldMapF_[FreeApplicative[F, ?], F](fold_(ap))))

  final def runSerialOrUnprincipled(implicit mo: Monad[F]): F[A] = run
  final def runSerialOrUnprincipled[G[_]](f: F ~> G)(implicit mo: Monad[G]): G[A] = compile(f).runSerialOrUnprincipled

  final def runParallel(implicit mo: Monad[F], pa: ParallelApplicative[F]): F[A] = run(mo, pa)
  final def runParallel[G[_]](f: F ~> G)(implicit mo: Monad[G], pa: ParallelApplicative[G]): G[A] = compile(f).runParallel

  final def shallowAnalyze[M](f: F ~> Lambda[B => M])(implicit m: Monoid[M]) =
    node.cata[Lambda[(G[_], B) => M]](Lambda[NodeSerialParallelF[Lambda[(G[_], B) => M], F, ?] ~> Lambda[B => M]](
      _.fold[Lambda[A => M]](f, Lambda[SerialParallelF[Lambda[(G[_], B) => M], F, ?] ~> Lambda[B => M]](
        _.resume(FreeApplicative.freeApplicative[Lambda[B => M]]).fold(
          _.analyze(FunctionK.id[Lambda[B => M]]), { _ => m.empty })))))
}