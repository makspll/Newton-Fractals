Installation

	cabal install BMP
	cabal install bytestring-strict-builder

For non gui version for generating AWESOME fractals (see makefile, has to be compiled with -threaded and -O2 flags)

	make

For gui version

	cabal install gtk

	On linux it may be beneficial to install gtk2hs

	make window


Running:
	./Fractals
