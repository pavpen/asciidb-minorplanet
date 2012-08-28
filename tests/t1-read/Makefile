NAME = t1

all: .capri
	capri install

.capri:
	capri bootstrap
	capri clone bytestring zlib \
		array deepseq containers binary \
		old-locale \
		filepath old-time unix directory pretty process Cabal \
		safe \
		text

dist-clean:
	rm -Rf dist .capri
