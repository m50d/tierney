# tierney

[![Build Status](https://travis-ci.org/m50d/tierney.svg?branch=master)](https://travis-ci.org/m50d/tierney)

Recursion schemes for Scala

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