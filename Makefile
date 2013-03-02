builddir=.

all: opt

doc:
	cabal configure
	cabal haddock
	cabal clean

debug:
	cabal configure --builddir ${builddir}
	cabal install --force-reinstall --builddir ${builddir} --ghc-options="-prof -rtsopts -caf-all -fprof-auto"

opt:
	cabal configure --builddir ${builddir}
	cabal install --enable-optimization --force-reinstall --builddir ${builddir}

clean:
	cabal clean
	rm -rf ${HOME}/builds/falling
