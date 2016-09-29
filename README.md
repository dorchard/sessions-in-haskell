# Artefact from the POPL'16 paper "Effects as Sessions, Sessions as Effects"
### by Dominic Orchard, Nobuko Yoshida (Imperial College London) ###

Effect and session type systems are two expressive behavioural type systems. The former is usually developed in the context of the lambda-calculus and its variants, the latter for the pi-calculus. In our paper we explored their relative expressive power. Firstly, we gave an embedding from PCF, augmented with a parameterised effect system, into a session-typed pi-calculus (session calculus), showing that session types are powerful enough to express effects. Secondly, we gave a reverse embedding, from the session calculus back into PCF, by instantiating PCF with concurrency primitives and its effect system with a session-like effect algebra (Section 6). The embedding of session types into an effect system is leveraged to give an implementation of session types in Haskell, via an effect system encoding (Section 7). *This is the artifact of this paper*, provided as the effect-sessions library.

Section 6 and 7 are the relevant parts of the paper for the artifact. As explained in Section 7, the implementation is not explicitly designed to be a user-friendly interface for programming with sessions. Instead, it is a fairly direct rendering in Haskell of the encoding of Section 6. Its aim is to give credance to embedding of Section 6 and to provide a new basis for working with session types in Haskell-- but is not a full "end product" in terms of user programming.

##### Limitations #####
There are a number of limitations in the implementation due to the limits of Haskell compared with the variant of PCF in Section 6. Mainly, Haskell has no subtyping or equirecursive types. The former is solved via explicit subtyping combinators in Haskell. The latter (equirecursive types) is solved via restricted forms of recursion, with fixed-points computed at the type-level only for affine effect equations. In practice, this covers a multitude of examples.

### Instructions

* Ensure you have GHC (Glasgow Haskell Compiler).
The latest version of GHC can be downloaded as part of the Haskell Platform for Windows, Linux, or Mac OS X.

* Install the stm package:
   cabal install stm
* Download the library sources
* The library also relies on the effect-monad and type-level-sets libraries. The relevant files from these are included with the download here for ease of use. The files comprising the contribution of the artifact are:
   - `Control/Effect/Sessions.hs`
   - `Control/Effect/Sessions/Process.lhs`
   - `Control/Effect/Sessions/Operations.lhs`
   - `Data/Type/FiniteMap.lhs`
   - `examples.lhs`
   - `effect-sessions.cabal`

* The examples.lhs file in the top-level directory provides a tutorial-like set of examples showing how to use the library. Open this in GHC interactive mode via:
~~~~
$ ghci examples.lhs
...
Main*>
~~~~
Then you are ready to try examples (by using run) and examine the types (e.g., executing :t simpleServer). Please follow the material in examples.lhs, which is a Literate Haskell file. This complements and expands on Section 7 of the paper.

### Documentation

The library has full Haddock [documentation here](http://www.cl.cam.ac.uk/~dao29/popl16/sessions/doc/html/effect-sessions/index.html "Documentation"). 
Most useful is the documentation for Control.Effect.Sessions.Operations which explains each combinator provided by the library for concurrency.
The main `examples.lhs` provides tutorial.

### Troubleshooting

If experiencing problems running the examples.lhs, try running `cabal configure` in the top-level directory which should explain more clearly any missing dependencies in your Haskell setup.

Whilst experimenting the library, the programmer may encounter some harder to read errors. This is a shortcoming of GHC type-level programming: it is hard to get good domain-specific errors. The following provides some hints.
A common cause of errors is not naming channels. Channels should be named whenever there is more than one channel being used in a computation block. If two endpoints of a channel are being bound in new then only one needs to be named.
Occasionally, extra type annotations are needed on primitive types to restrict them from polymorphic to monomorphic instances. e.g., write (x :: Int) <- recv c instead of just x <- recv c.
Check that the error isn't an expected error: e.g., the types of the process don't match up, or a balancing check fails, e.g. NotBal (Bal s).

### Notes

effect-sessions has been tested on the following architectures/versions:
  - Mac OS X 10.9.5: GHC 7.8.2, GHC 7.10.1, GHC 8.0.1
  - Windows 7, GHC 7.10.2
  - Ubuntu 15.04, GHC 7.10.2
effect-sessions relies on the effect-monad and type-level-sets libraries. These are included with the download here for ease of use. These can also be installed via cabal (cabal install effect-monad, cabal install type-level-sets).
effect-sessions itself can be installed (though this isn't necessary for experimenting with the examples) via:
~~~~
 cabal configure
 cabal build
 cabal install
~~~~
 
Depending on your setup, the last command 'cabal install' may require super-user privileges.

### Acknowledgments

Thanks to Julien Lange and Bernardo Toninho for their comments and testing. Any remaining issues are my own.
