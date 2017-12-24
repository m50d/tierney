package tierney

import cats.data.EitherK
import cats.free.FreeApplicative
import cats.free.Free
import tierney.core._
import cats.~>
import cats.kernel.Monoid

package object free extends EitherKSupport with FreeSupport with FreeApplicativeSupport {
  /** Either an immediate command F or a recursive S structure
   */
  type NodeF[S[_[_], _], F[_], A] = EitherK[F, S[F, ?], A]
  object NodeF {
    implicit val functorKKNodeF: FunctorKK[NodeF] = new FunctorKK[NodeF] {
      override def map[S[_[_], _], T[_[_], _], F[_]](f: S[F, ?] ~> T[F, ?]) =
        rightMap[F, S[F, ?], T[F, ?]](f)
    }
    def left[S[_[_], _], F[_], A](f: F[A]): NodeF[S, F, A] = EitherK.leftc[F, S[F, ?], A](f)
    def right[S[_[_], _], F[_], A](s: S[F, A]): NodeF[S, F, A] = EitherK.rightc[F, S[F, ?], A](s)
  }
  
  /** A fan of S constructs to execute in parallel
   */
  type ParallelF[S[_[_], _], F[_], A] = FreeApplicative[S[F, ?], A]
  object ParallelF {
    implicit val functorKKParallelF: FunctorKK[ParallelF] = new FunctorKK[ParallelF] {
      override def map[S[_[_], _], T[_[_], _], F[_]](f: S[F, ?] ~> T[F, ?]) = 
        compile_[S[F, ?], T[F, ?]](f)
    }
  }
  
  /** A chain of S constructs to execute serially
   */
  type SerialF[S[_[_], _], F[_], A] = Free[S[F, ?], A]
  object SerialF {
    implicit val functorKKSerialF: FunctorKK[SerialF] = new FunctorKK[SerialF] {
      override def map[S[_[_], _], T[_[_], _], F[_]](f: S[F, ?] ~> T[F, ?]) = compileF_[S[F, ?], T[F, ?]](f)
    }
  }
  
  /** Either an immediate command F or a chain of fans of S constructs
   */
  type NodeSerialParallelF[S[_[_], _], F[_], A] = 
    NodeF[Lambda[(G[_], B) => SerialF[Lambda[(H[_], C) => ParallelF[Lambda[(I[_], D) => S[I, D]], H, C]], G, B]], F, A]
  object NodeSerialParallelF {
    implicit val functorKKNodeSerialParallelF : FunctorKK[NodeSerialParallelF] =
      ParallelF.functorKKParallelF andThen SerialF.functorKKSerialF andThen NodeF.functorKKNodeF
  }
  
  /** "Unfixed" variant of node - logically isomorphic but a different representation.
   * The same type as Node_[Node, F, A]
   */
  private[this] type UNode[F[_], A] = NodeF[Serial, F, A]
  private[this] implicit def functorKUNode: FunctorK[UNode] = new FunctorK[UNode] {
    override def map[F[_], G[_]](f: F ~> G) =
      foldCP_[F, Serial[F, ?], UNode[G, ?]](
        f andThen[UNode[G, ?]] left_[G, Serial[G, ?]],
        functorKSerial.map(f) andThen[UNode[G, ?]] right_[G, Serial[G, ?]]
      )
  }
  
  /** Either an immediate command F or a chain of fans of nodes
   */
  type Node[F[_], A] = FixKK[NodeSerialParallelF, F, A]
  object Node {
    def left[F[_], A](f: F[A]): Node[F, A] = new FixKK[NodeSerialParallelF, F, A](
      NodeF.left[Lambda[(G[_], B) => SerialF[Lambda[(H[_], C) => ParallelF[Lambda[(I[_], D) => FixKK[NodeSerialParallelF, I, D]], H, C]], G, B]], F, A](f))
    def right[F[_], A](s: Serial[F, A]): Node[F, A] = new FixKK[NodeSerialParallelF, F, A](NodeF.right[Serial, F, A](s)) 
    def apply[F[_], A](f: F[A]): Node[F, A] = left(f) 
  }
  implicit def functorKNode: FunctorK[Node] = new FunctorK[Node] {
    override def map[F[_], G[_]](f: F ~> G) =
      unfixKK[NodeSerialParallelF, F] andThen[UNode[G, ?]]
        (new LazyFunctionK[UNode[F, ?], UNode[G, ?]](functorKUNode.map(f))) andThen[Node[G, ?]]
      fixKK[NodeSerialParallelF, G]
  }
  final implicit class NodeOps[F[_], A](override val node: Node[F, A]) extends AnyVal with TierneyFree[F, A]
  
  /** A fan of nodes to execute in parallel
   */
  type Parallel[F[_], A] = ParallelF[Node, F, A]
  object Parallel {
    def apply[F[_], A](command: F[A]): Parallel[F, A] = Node(command).parallel
  }
  implicit def functorKParallel: FunctorK[Parallel] = new FunctorK[Parallel] {
    override def map[F[_], G[_]](f: F ~> G) =
      compile_[Node[F, ?], Node[G, ?]](functorKNode.map(f))
  }
  final implicit class ParallelOps[F[_], A](override val parallel: Parallel[F, A]) extends AnyVal with TierneyFree[F, A] {
    def shallowAnalyze[M](f: F ~> Lambda[B => M])(implicit m: Monoid[M]) =
      parallel.analyze(Lambda[Node[F, ?] ~> Lambda[B => M]](
          _.unfix.fold[Lambda[A => M]](f, Lambda[Serial[F, ?] ~> Lambda[B => M]](_ => m.empty))))
  }
  
  /** A chain of fans of parallel and serial F commands
   */
  type Serial[F[_], A] = SerialF[Parallel, F, A]
  object Serial {
    def apply[F[_], A](command: F[A]): Serial[F, A] = Node(command).serial
  }
  implicit def functorKSerial: FunctorK[Serial] = new FunctorK[Serial] {
    override def map[F[_], G[_]](f: F ~> G) = compileF_[Parallel[F, ?], Parallel[G, ?]](functorKParallel.map(f))
  }
  final implicit class SerialOps[F[_], A](override val serial: Serial[F, A]) extends AnyVal with TierneyFree[F, A]
}