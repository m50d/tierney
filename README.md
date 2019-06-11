# Tierney

Generic library for structured commands with explicit parallelism

[![Build Status](https://travis-ci.org/m50d/tierney.svg?branch=master)](https://travis-ci.org/m50d/tierney)

Combining Free Monads and Free Applicatives is a great way to get the advantages of both, as seen in e.g.
[this talk by Markus Hauck](https://github.com/markus1189/flatmap-oslo-2016).
Tierney offers some simple wrappers to reduce the boilerplate of doing this with your own types,
while also enforcing that code is always very explicit about which steps can and can't happen in parallel,
freeing you to refactor with confidence that a simple change isn't going to accidentally serialise
all of your operations.
 
## How to use

### Add the dependency

    <dependency>
		<groupId>com.github.m50d</groupId>
		<artifactId>tierney-free</artifactId>
		<version>1.5</version>
	</dependency>

### Direct use with `Future` or `Task`

Given a "deferred execution" type like FS2 `Task`, Tierney lets you write code that's more explicit
about which things may happen in serial versus in parallel:

    import tierney.free._, cats.syntax.all._
    (for { 
      w <- ( (for {
          x <- Serial(someTask)
          y <- Serial(anotherTask(x)) 
        } yield y).parallel,
          Parallel(yetAnotherTask) ).mapN {
            (y, z) => someMerge(y, z)
          } .serial
      v <- ...
    } yield {...}).runParallel

`y` will be computed after `x`, but `z` will be computed in parallel with both; attempting to use
`for`/`yield` on `Parallel`s is a compilation error, while using `Applicative` functionality
like `mapN` on `Serial`s will serialize their evaluation.
This means we leave some performance on the table compared to "opportunistic" approaches
that offer both parallel `mapN` and serial `for`/`yield` on the same type,
but we ensure that equivalent code (according to the monad/applicative laws) has equivalent parallelism,
so "safe" refactors should never radically change performance characteristics (in either direction).
The only way to change parallel code to serial (or vice versa) is to change `Serial(x)` to 
`Parallel(x)` or add/remove an explicit `.serial` or `.parallel` call.
Hopefully these strike the right balance between lightweight and explicit. 

### Use with custom command type and interpreter

Tierney can be used with `F[_]` being a custom command type - using `TierneyFree[F, A]`
like you'd use `Free[F, A]`. In this case you simply pass an interpreter to the final
`runParallel` call: 

    sealed trait MyCommand[A]
    case class ReadInt(path: String) extends MyCommand[Int]
    case class WriteInt(path: String, value: Int) extends MyCommand[Unit]
    ...
    
    val program = for {
      i <- Serial(ReadInt("src"))
      _ <- (Parallel(WriteInt("dst1", i)) *> Parallel(WriteInt("dst2", i + 4))).serial
    } yield {}
    
    object MyInterpreter extends (MyCommand ~> Task) { ... }
    
    program.runParallel(MyInterpreter)

See [this example code](https://github.com/m50d/tierney/tree/master/free/src/test/scala/tierney/free/github)
for a more in-depth example that corresponds to [Markus Hauck's talk](https://github.com/markus1189/flatmap-oslo-2016).

### Reference

   * Conceptually there are three mutually recursive `TierneyFree`  subtypes:
      * `Node[F, A] = Coproduct[F, Serial[F, ?], A]`
         * `Left` `Node`s are where the recursive structure bottoms out in actual `F[_]`s
         * The real type is slightly different since we do the recursion using
         a fixed point combinator `FixKK` rather than directly
         * The type above is called `UNode`
      * `Parallel[F, A] = FreeApplicative[Node[F, ?], A]`
         * Can be used with `mapN`-style composition
      * `Serial[F, A] = Free[Parallel[F, ?], A`
         * Can be used with `for`/`yield` composition
   * Any `TierneyFree` can be converted to any of the three types using `.node`, `.serial` and `.parallel` 
   * Interpretation functions are on `TierneyFree` itself
      * `runParallel` requires both a `Monad` (for executing serial sections) and a `ParallelApplicative`,
      a custom type that represents `Applicative`s that we know run operations in parallel. This is really
      "run as parallel as possible" - serial commands can only ever be executed serially.
      * `runSerialOrUnprincipled` executes both serial and parallel commands using the passed `Monad`.
      Therefore every command will run serially, unless the `Monad` uses unprincipled "opportunistic"
      parallelisation.
      * `shallowAnalyze` will analyze "as far as possible" i.e. it will "hide" any chained
      serial commands. E.g. if you're trying to prefetch users, this can only let you
      prefetch those users whose IDs were passed in to start with, not e.g. friends
      of those users, since it's not possible to find out their user IDs without
      actually running the commands.

## Information for developers

### Desirable features

 * Add a deeper equivalent of `shallowAnalyze` to enable us to optimize code after `flatMap` chains
 * Look at integrating with cats `Parallel` (perhaps renaming our `Parallel`) if it ever takes off.
 * Replace current basic performance test of `ParallelApplicative` with something more robust
 * Add more `ParallelApplicative` implementations e.g. scalaz-concurrent `Task`
 * Add performance tests for all `ParallelApplicative` implementations
 * Add `tut-maven-plugin` (once implemented) to enforce that code examples in this document are correct

### Current project layout

 * Higher-kinded variants of some general constructs as necessary to support the rest of the project
   (`core`, potentially to be replaced with use of polykinds from Typelevel Scala)
 * Recursion-schemes style `Fix`/`FixK`/... constructs and traversal implementations
   (currently also `core`, potentially to be spun out into a separate module if the above remains necessary)
 * `ParallelApplicative` type to express explicit, intentional parallelism
   (`free`, potentially to be spun out into a separate module)
   * Implementations for common cases (stdlib `Future`, monix `Task`, fs2 `Task`),
     potentially to be spun out into their own modules to minimize dependencies
 * Implementation of a free hybrid monad-applicative structure, making use of all the above (`free`)
 
### How to develop in Eclipse

 1. Usual Eclipse Scala development setup:
  1. Unzip Eclipse
  1. Install scala-ide plugin
  1. Install m2eclipse-scala plugin
  1. Install maven scm connector for git (possibly bundled in recent versions of eclipse)
 1. File > Import ... , Maven > Check Out Maven Projects from SCM
 1. Enter the clone URL from GitHub
 1. Click Next, then Finish immediately i.e. skip as much of the wizard as possible
 1. For each project:
  1. Right click > Properties, Scala Compiler
   1. Advanced tab, XPlugin box: fill in the full path to `kind-projector[...].jar`
   1. Build manager tab, untick "withVersionClasspathValidator" as it is overly sensitive to patch versions for compiler plugins