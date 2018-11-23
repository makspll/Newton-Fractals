# Installation
Prequisites
* `cabal install BMP`
* `cabal install bytestring-strict-builder`

## Non-gui version

#### Linux

	make

#### Windows

`make` also works, but if it did't use:

`ghc --make main.hs -o Fractals -threaded -cpp -O2`
	

## For gui version

`cabal install gtk`

On linux it may be beneficial to install `gtk2hs`

`make window`

#### Windows

if `make` doesn't work use:

`ghc --make main.hs -o Fractals -threaded -cpp -D__GUI_APP -O2`

It should create executable of name: `Fractals`
