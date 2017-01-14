package tierney

import cats.data.Coproduct
import cats.free.FreeApplicative
import cats.free.Free

package object free {
  type TierneyFreeF[S[_], F[_], A] = Coproduct[Free[S, ?], FreeApplicative[Coproduct[F, S, ?], ?], A] 
}