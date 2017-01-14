package tierney

import cats.data.Coproduct
import cats.free.FreeApplicative
import cats.free.Free

package object free {
  type TierneyFreeF[S[_[_], _], F[_], A] = Coproduct[Free[S[F, ?], ?], FreeApplicative[Coproduct[F, S[F, ?], ?], ?], A]
}