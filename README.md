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

How to profile
--------------
One of points that code try to illustrate is how easy it is work with lazy steams. Thus, we write the code as if we read in the whole file, but due to laziness we only use constant memory independent of the size of the input file.

How can check that code really behaves as we intend? By using the space profiler:

    $ ghc -O3 --make -prof -auto-all -caf-all readbits.hs -o readbits-hs
    $ ./readbits-hs <infile> <outfile> +RTS -hc -p
	$ hp2ps -e8in -c readbits-hs.hp && open readbits-hs.ps
	
