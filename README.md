# Installation
Prequisites
* `cabal install BMP`
* `cabal install bytestring-strict-builder`

## Non-GUI version

#### Linux

	make

#### Windows

`make` also works, but if it doesn't use:

`ghc --make main.hs -o Fractals -threaded -cpp -O2`


## For GUI version

`cabal install gtk`

On linux it may be beneficial to install `gtk2hs`

`make window`

#### Windows

if `make` doesn't work use:

`ghc --make main.hs -o Fractals -threaded -cpp -D__GUI_APP -O2`

It should create an executable called: `Fractals`
