FILES = main.hs 

EXEC = Fractals

FLAGS = --make -threaded -cpp
DEBUG = -prof -fprof-auto +RTS -p -hy
RELEASE = -O2
WINDOW = -D__GUI_APP

GHC = ghc

all: $(FILES)
	$(GHC) $(FLAGS) $^ -o $(EXEC) $(RELEASE)

debug: $(FILES)
	$(GHC) $(FLAGS) $^ -o $(EXEC) $(DEBUG) $(RELEASE)
	@echo "generating tables"
	$(GHC) hp2ps -c $(EXEC).hp

window: $(FILES)
	$(GHC) $(FLAGS) $^ -o $(EXEC) $(RELEASE) $(WINDOW)
