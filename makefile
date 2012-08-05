all: haskulele clean

haskulele:
	ghc Haskulele

clean:
	rm -f *.o *.hi