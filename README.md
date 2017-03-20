# tierney

Generic library for structured commands with explicit parallelism

[![Build Status](https://travis-ci.org/m50d/tierney.svg?branch=master)](https://travis-ci.org/m50d/tierney)

Combining Free Monads and Free Applicatives is a great way to get the advantages of both, as seen in e.g.
[this talk by Markus Hauck](https://speakerdeck.com/markus1189/free-monads-and-free-applicatives).
Tierney offers some simple wrappers to reduce the boilerplate of doing this with your own types,
while also enforcing that code is always very explicit about which steps can and can't happen in parallel,
freeing you to refactor with confidence that a simple change isn't going to accidentally serialise
all of your operations. 
 
## How to use

### Add the dependency

    <dependency>
		<groupId>com.github.m50d</groupId>
		<artifactId>tierney-free</artifactId>
		<version>0.2</version>
	</dependency>

### Direct use with `Future` or `Task`

TODO

### Use with custom command type and interpreter

TODO

## Information for developers

### Design

 * Conceptually there are three mutually recursive `TierneyFree` types:
   * `Node[F, A] = Coproduct[F, Serial[F, ?], A`
   * `Parallel[F, A] = FreeApplicative[Node[F, ?], A]`
   * `Serial[F, A] = Free[Parallel[F, ?], A`
 * The recursion is implemented using a fixed point combinator `FixKK` rather than directly
   * `Node` therefore actually has a slightly different type; the type above is called `UNode`

### Features required for 1.0

 * Write examples in the this document
 
### Other desirable features

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