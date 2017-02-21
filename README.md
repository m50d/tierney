# tierney

[![Build Status](https://travis-ci.org/m50d/tierney.svg?branch=master)](https://travis-ci.org/m50d/tierney)

A project exploring recursion-schemes style and higher-kinded constructs in Scala:

 * Higher-kinded variants of some general constructs as necessary to support the rest of the project
   (`core`, potentially to be replaced with use of polykinds from Typelevel Scala)
 * Recursion-schemes style `Fix`/`FixK`/... constructs and traversal implementations
   (currently also `core`, potentially to be spun out into a separate module if the above remains necessary)
 * Implementation of a free hybrid monad-applicative structure, making use of the above (`free`) 

## Developing in Eclipse

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