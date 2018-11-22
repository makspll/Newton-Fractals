FILES = ver1.hs 

EXEC = prog

FLAGS = --make -threaded
DEBUG = -prof -fprof-auto +RTS -p -hy
RELEASE = -O2
WINDOW = -D__GUI_APP # window.hs

GHC = ghc

all: $(FILES)
	$(GHC) --make $(FLAGS) $^ -o $(EXEC) -cpp $(RELEASE)

debug: $(FILES)
	$(GHC) --make $(FLAGS) $^ -o $(EXEC) -cpp $(DEBUG) $(RELEASE)
	@echo "generating tables"
	$(GHC) hp2ps -c $(EXEC).hp

window: $(FILES)
	$(GHC) --make $(FLAGS) $^ -o $(EXEC) -cpp $(RELEASE) $(WINDOW)
