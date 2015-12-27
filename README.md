# Write Yourself a Scheme (Interpreter)

The goal of this project is to write a near-minimal implementation
of lisp/scheme in haskell.

## Installation

Clone the git repo and then create a cabal sandbox using

`cabal sandbox init`

If you want to enable testing, do

`cabal install --enable-tests --only-dependencies`
`cabal configure --enable-tests`
