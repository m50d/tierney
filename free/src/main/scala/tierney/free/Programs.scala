object Demo {
  trait Free1[F[_], A]
  trait Free2[F[_], A]
  trait Cp[F[_], G[_], A]

  class Fix[W[_[_[_], _], _[_], _], F[_], A]()
  type Unfixed[S[_[_], _], F[_], A] = 
    Cp[F, Free1[Free2[S[F, ?], ?], ?], A]
  
  trait Command[A]
  
  def unify[F[_], A](value: F[A]): Unit = {}
  
  def testSuccess(v: Free2[Fix[Unfixed, Command, ?], String]) =
    unify[Free2[Fix[Unfixed, Command, ?], ?], String](v)
    
  def testFail(v: Free2[Fix[Unfixed, Command, ?], String]) =
    unify(v)
}

