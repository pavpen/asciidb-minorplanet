NAME = t4

PKG_MGR=cabal-dev
#PKG_MGR=capri

all: .$(PKG_MGR)-install

.capri:
	capri bootstrap
	capri clone bytestring zlib \
		array deepseq containers binary \
		old-locale \
		filepath old-time unix directory pretty process Cabal \
		safe \
		text time
	capri import ../..

.capri-install: .capri
	capri install

cabal-dev:
	#cabal-dev add-source ../..

.cabal-dev-install:
	cabal-dev install . ../..

dist-clean:
	rm -Rf dist .capri cabal-dev
