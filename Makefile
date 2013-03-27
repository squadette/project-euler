sources = $(wildcard problem*.hs)
binaries = $(basename $(sources))

problem% : problem%.hs
	ghc --make $<

.PHONY: all
all: $(binaries)

.PHONY: clean
clean:
	rm -f problem??? *.o *.hi
