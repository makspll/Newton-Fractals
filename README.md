# Overview
The program was the winning submission for the 2018 Haskell Functional Programming Competition @ UoE. The task was to use the skills we learned and apply them to anything fractal related.

(Fractals & Documentation)[https://drive.google.com/file/d/0B8EogF1phOCBM19LOWd5M3o2N25xdklmWllfaEtfYkZOeGRz/view]
# Installation
Prequisiteshttps://drive.google.com/file/d/0B8EogF1phOCBM19LOWd5M3o2N25xdklmWllfaEtfYkZOeGRz/view
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
