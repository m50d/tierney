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
    implicit def functorKK: FunctorKK[TierneyFreeF] = new FunctorKK[TierneyFreeF] {
      override def map[S[_[_], _], T[_[_], _]](f: S ~~> T): Lambda[(F[_], A) ⇒ TierneyFreeF[S, F, A]] ~~> Lambda[(F[_], A) ⇒ TierneyFreeF[T, F, A]] =
        new (Lambda[(F[_], A) ⇒ TierneyFreeF[S, F, A]] ~~> Lambda[(F[_], A) ⇒ TierneyFreeF[T, F, A]]) {
          override def apply[F[_]]: TierneyFreeF[S, F, ?] ~> TierneyFreeF[T, F, ?] = new (TierneyFreeF[S, F, ?] ~> TierneyFreeF[T, F, ?]) {
            override def apply[A](fa: TierneyFreeF[S, F, A]) = fa.fold[TierneyFreeF[T, F, ?]](
              new (Free[S[F, ?], ?] ~> TierneyFreeF[T, F, ?]) {
                override def apply[B](fb: Free[S[F, ?], B]) =
                  Coproduct.leftc[Free[T[F, ?], ?], FreeApplicative[Coproduct[F, T[F, ?], ?], ?], B](fb.compile[T[F, ?]](f[F]))
              }, new (FreeApplicative[Coproduct[F, S[F, ?], ?], ?] ~> TierneyFreeF[T, F, ?]) {
                override def apply[B](fb: FreeApplicative[Coproduct[F, S[F, ?], ?], B]) =
                  Coproduct.rightc[Free[T[F, ?], ?], FreeApplicative[Coproduct[F, T[F, ?], ?], ?], B](
                    fb.compile[Coproduct[F, T[F, ?], ?]](new (Coproduct[F, S[F, ?], ?] ~> Coproduct[F, T[F, ?], ?]) {
                      override def apply[C](fc: Coproduct[F, S[F, ?], C]) =
                        fc.fold[Coproduct[F, T[F, ?], ?]](new (F ~> Coproduct[F, T[F, ?], ?]) {
                          override def apply[D](fd: F[D]) = Coproduct.leftc[F, T[F, ?], D](fd)
                        }, new (S[F, ?] ~> Coproduct[F, T[F, ?], ?]) {
                          override def apply[D](fd: S[F, D]) = Coproduct.rightc[F, T[F, ?], D](f[F](fd))
                        })
                    }))
              })
          }
        }
    }
  }
}