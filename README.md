What is it?
===========

Example code showing how to use the ByteString library to work with binary data in Haskell.

How to build
------------
You can either use cabal:

    $ cabal configure
    $ cabal build
    $ ./dist/build/readbits-hs/readbits-hs infile outfile

Or you can just use `ghc` directly:

    $ ghc -O3 --make readbits.hs -o readbits-hs
    $ ./readbits-hs infile outfile