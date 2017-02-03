package tierney

import cats.data.Coproduct
import cats.free.FreeApplicative
import cats.free.Free
import tierney.core.FunctorKK
import cats.~>
import tierney.core.~~>

package object free {
  type TierneyFreeF[S[_[_], _], F[_], A] = Coproduct[Free[S[F, ?], ?], FreeApplicative[Coproduct[F, S[F, ?], ?], ?], A]
  object TierneyFreeF {
//    implicit def functorKK: FunctorKK[TierneyFreeF] = new FunctorKK[TierneyFreeF] {
//      override def map[S[_[_], _], T[_[_], _]](f: S ~~> T): Lambda[(F[_], A) => TierneyFreeF[S, F, A]] ~~> Lambda[(F[_], A) => TierneyFreeF[T, F, A]] =
//        new (Lambda[(F[_], A) => TierneyFreeF[S, F, A]] ~~> Lambda[(F[_], A) => TierneyFreeF[T, F, A]]) {
//        override def apply[F[_]]: TierneyFreeF[S, F, ?] ~> TierneyFreeF[T, F, ?] = new (TierneyFreeF[S, F, ?] ~> TierneyFreeF[T, F, ?])   {
//          override def apply[A](fa: TierneyFreeF[S, F, A]) = fa.fold[TierneyFreeF[T, F, ?]](
//            new (Free[S[F, ?], ? ~> )    
//          )
//        }
//      }
//    }
  }
}