Authors
=======

François  Ripault
Sébastien Crozet

HBDD
====

Purely functional ROBDD library.

The library sources are on the 'Data/HBDD' folder.
The demos are on the 'Demos' folder.

Since HBDD is purely functional and ROBDDs need some global state to store BDDs, the user has two
ways to achieve state (context)-passing:
  * using the direct api (see Operations.hs) and explicit state passing. Having to passe the state
    explicitly forbids the use of infix operators, and is very error-prone. So, you might prefer
    the State-based version.
  * using the api wrapped of the State monad (see HBDDState.hs). Using the state monad, all the
    context-passing is handled seemlessly and all operators can be gracefully used as infix
    operators. The only drawback is that, to do get the result of the computation, you have to push
    your BDD out of State. Dont forget to use runState and execState for that.

Installation
============

The whole instalation processe is managed by the 'cabal' command line (cabal-install package).
If not installed, the haskell-platform is needed. The cabal command line is shiped with it:

> apt-get install haskell-platform

Then, update cabal’s repository list (to be able to download dependencies from hackage):

> cabal update

Finally go to the root of the project and run the compilation:

> cabal configure
> cabal install

The configure step can take a while since it will compile all the project’s dependencies.

This will install HBDD and its demoes on an user-specific folder. It is very likely to be on the
'~/.cabal/bin' folder.

Run the demos with:

> ~/.cabal/bin/hbdd-8queens -sat 8
> ~/.cabal/bin/hbdd-knights 6
> ~/.cabal/bin/hbdd-colormap (and hope this will terminate some day)
> ~/.cabal/bin/hbdd-colormap-simpl

The 'hbdd-colormap-simpl' demo is a smaller version of the usa coloring problem with only a few
states located of the West.

A small documentation is provided with the package. Generate it with:

> cabal haddock

Then open the file './dist/doc/html/hbdd/index.html'.

If you want to cleanup the projet, use:

> cabal clean
