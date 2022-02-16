[![Hackage](https://img.shields.io/hackage/v/ip.svg)](https://hackage.haskell.org/package/ip)

# Instructions

Look at the [haddocks](http://hackage.haskell.org/package/ip) for this 
package to learn how to use it.

# Contributing

Most contributions are welcome, especially performance improvements in encoding and decoding of Text/ByteString.
Please make sure to follow naming conventions followed in the modules.

## Doctest

Doctest used to be provided as a test suite, but `doctest-0.20` and higher
do not require this to be run. To run the doctests, make sure you have
`doctest` on your path (i.e. run `cabal install doctest`), and then run:

    cabal build
    cabal repl --build-depends=QuickCheck --with-ghc=doctest --repl-options='-fno-warn-orphans'

This runs incredibly slowly, but it works for now. Doctest is not run by CI,
so if you make a change that adds more doctests, it needs to be run by hand
by someone. (The maintainer is happy to do this if you're on a platform
where doctest is finicky.)
