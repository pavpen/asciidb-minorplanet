NAME = t1

PKG_MGR=cabal-dev
#PKG_MGR=capri

all: .$(PKG_MGR)-install

.capri:
	capri bootstrap
	capri clone array deepseq time bytestring zlib containers binary \
		old-locale \
		filepath old-time unix directory pretty process Cabal \
		safe \
		text
	capri import ../..

.capri-install: .capri
	capri install

cabal-dev:
	#cabal-dev add-source ../..

.cabal-dev-install: cabal-dev
	cabal-dev install . ../..

dist-clean:
	rm -Rf dist .capri cabal-dev
