package tierney.free.github

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import cats.~>
import cats.instances.future._
import cats.instances.list._
import cats.instances.set._
import cats.syntax.apply._
import cats.syntax.flatMap._
import cats.syntax.traverse._
import scala.concurrent.duration._
import tierney.free._
import cats.Applicative
import cats.Functor
import tierney.parallel.ParallelApplicative
import cats.Monad
import scala.concurrent.Future
import scala.concurrent.Await

trait ApplicativePrograms {
  import GitHubDsl._
  def getUsers(logins: List[UserLogin]): Parallel[GitHub, List[User]] =
    logins.traverse(getUser)

  def logins: Parallel[GitHub, List[User]] =
    List(UserLogin("markus1189"), UserLogin("..."), ???).
      traverse(login => getUser(login))

  val issuesConcat: Parallel[GitHub, List[Issue]] =
    (
      listIssues(Owner("scala"), Repo("scala-dev")),
      listIssues(Owner("scala"), Repo("slip"))).mapN(_ ++ _)

  val scalaIssues: Parallel[GitHub, List[Issue]] =
    List("scala", "scala-dev", "slip", "scala-lang").
      traverse(repo =>
        listIssues(Owner("scala"), Repo(repo))).
      map(_.flatten)

  def extractLogins(p: TierneyFree[GitHub, _]): Set[UserLogin] = {
    import GitHubInterp._
    p.shallowAnalyze(requestedLogins)
  }

  def precompute[A, F[_]: ParallelApplicative: Monad](
    p: TierneyFree[GitHub, A],
    interp: GitHub ~> F): F[Map[UserLogin, User]] = {
    val userLogins = extractLogins(p).toList

    val fetched: F[List[User]] =
      userLogins.traverse(getUser).runParallel(interp)

    Monad[F].map(fetched)(userLogins.zip(_).toMap)
  }

  def optimizeNat[F[_]: Applicative](
    mapping: Map[UserLogin, User],
    interp: GitHub ~> F): GitHub ~> F = new (GitHub ~> F) {
    def apply[A](fa: GitHub[A]): F[A] = fa match {
      case ffa @ GetUser(login) =>
        mapping.get(login) match {
          case Some(user) => Applicative[F].pure(user)
          case None => interp(ffa)
        }
      case _ => interp(fa)
    }
  }

  def interpret: GitHub ~> Future
  def interpretOpt[A](p: TierneyFree[GitHub, A])(implicit ec: scala.concurrent.ExecutionContext): Future[A] = {
    val mapping: Future[Map[UserLogin, User]] = precompute(p, interpret)

    mapping.flatMap { m =>
      val betterNat = optimizeNat(m, interpret)
      p.runParallel(betterNat)
    }
  }
}

trait Programs {
  import GitHubDsl._

  def branching =
    listIssuesMonad(Owner("foo"), Repo("bar")).map(_.nonEmpty).ifM(
      listIssuesMonad(Owner("foo"), Repo("baz")) //if true
      , listIssuesMonad(Owner("foo"), Repo("qux"))) //if false

  def allUsers(
    owner: Owner,
    repo: Repo): Serial[GitHub, List[(Issue, List[(Comment, User)])]] = for {
    issues <- listIssuesMonad(owner, repo)

    issueComments <- issues.traverse(issue =>
      getCommentsMonad(owner, repo, issue).map((issue, _)))

    users <- issueComments.traverse {
      case (issue, comments) =>
        comments.traverse(comment =>
          getUserMonad(comment.user).map((comment, _))).map((issue, _))
    }
  } yield users

  def allUsersM(owner: Owner, repo: Repo): Serial[GitHub, List[(Issue, List[(Comment, User)])]] = for {

    issues <- listIssuesM(owner, repo)

    issueComments <- issues.traverse(issue =>
      getComments(owner, repo, issue).map((issue, _))).serial

    users <- issueComments.traverse {
      case (issue, comments) =>
        comments.traverse(comment =>
          getUser(comment.user).map((comment, _))).map((issue, _))
    }.serial
  } yield users

  def userNamesFromIssueComments(
    owner: Owner,
    repo: Repo,
    issue: Issue): Serial[GitHub, List[User]] = for {
    comments <- getCommentsM(owner, repo, issue)
    users <- comments.traverse(comment => getUser(comment.user)).serial
  } yield users

  def userNamesFromAllIssuesComments(
    owner: Owner,
    repo: Repo): Serial[GitHub, List[List[User]]] = for {
    issues <- listIssuesM(owner, repo)
    issueComments <- issues.traverse(issue => getComments(owner, repo, issue)).serial
    users <- getUsers(issueComments).serial
  } yield users

  def getUsers(issueComments: List[List[Comment]]): Parallel[GitHub, List[List[User]]] =
    issueComments.traverse(comments =>
      comments.traverse(comment =>
        getUser(comment.user)))
}

object Webclient {
  implicit val ec = scala.concurrent.ExecutionContext.Implicits.global // don't do this
  import GitHubDsl._

  private def withClient[A](f: Client => A): A = {
    implicit val sys: ActorSystem = ActorSystem(s"github-run-${util.Random.nextInt.abs}")
    implicit val mat: ActorMaterializer = ActorMaterializer()
    val client: Client = Client.ahcws

    try {
      f(client)
    } finally {
      client.close()
      mat.shutdown()
      sys.terminate()
    }
  }

  val timeOut: FiniteDuration = 5 minutes

  def applicative[A](p: TierneyFree[GitHub, A]): A = {
    import GitHubInterp._
    withClient { client =>
      Await.result(p.runParallel(step(client)), 5.minutes)
    }
  }

  def monadic[A](p: TierneyFree[GitHub, A]): A = {
    import GitHubInterp._
    withClient { client =>
      Await.result(p.runSerialOrUnprincipled(step(client)), 5.minutes)
    }
  }

  def apply[A](
    p: TierneyFree[GitHub, A],
    dur: FiniteDuration = timeOut,
    doApplicative: Boolean = false,
    doMonadic: Boolean = false,
    doOptimized: Boolean = false): Unit = {
    withClient { client =>

      if (doApplicative) {
        val parallel: TierneyFree[GitHub, ?] ~> Future = {
          import GitHubInterp._
          Lambda[TierneyFree[GitHub, ?] ~> Future](_.runParallel(step(client)))
        }

        println("©" * 80)
        println("Applicative program (sleeping a moment):")
        Thread.sleep(2000)
        val resA = parallel(p)

        Await.result(resA, timeOut)
      }

      if (doMonadic) {
        val sequential: TierneyFree[GitHub, ?] ~> Future = {
          import GitHubInterp._
          Lambda[TierneyFree[GitHub, ?] ~> Future](_.runSerialOrUnprincipled(step(client)))
        }
        println("©" * 80)
        println("Monadic program:")
        val resM = sequential(p)

        Await.result(resM, timeOut)
      }

      if (doOptimized) {
        val optimized: TierneyFree[GitHub, ?] ~> Future = {
          import GitHubInterp._
          val ap = new ApplicativePrograms {
            override def interpret = step(client)
          }
          Lambda[TierneyFree[GitHub, ?] ~> Future](ap.interpretOpt(_))
        }

        println("©" * 80)
        println("Applicative program OPTIMIZED (sleeping a moment):")
        Thread.sleep(2000)
        val resOpt = optimized(p)

        Await.result(resOpt, timeOut)
      }
    }
  }
}

object MonadicDsl extends Programs {
  def main(args: Array[String]): Unit =
    println(Webclient.monadic(allUsers(Owner("scala"), Repo("scala"))))
}

object ApplicativeDsl extends ApplicativePrograms {
  def main(args: Array[String]): Unit =
    println(Webclient.applicative(scalaIssues))

  def interpret: GitHub ~> Future = ???
}

object MixedDsl extends Programs {
  def main(args: Array[String]): Unit =
    println(Webclient.applicative(allUsersM(Owner("scala"), Repo("scala"))))
}
