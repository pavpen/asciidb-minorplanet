asciidb-minorplanet
===================

(C) 2012 Pavel M. Penev

License: GPL

Haskell functionality for reading and writing ASCII minor planet (e.g.
asteroid) databases.

The Minor Planet Center's [observations
databases](http://www.minorplanetcenter.net/iau/ECS/MPCAT-OBS/MPCAT-OBS.html)
are supported as much as I could figure out their format.  The Lowell
Observatory's asteroid orbits database
([ftp://ftp.lowell.edu/pub/elgb/astorb.html](ftp://ftp.lowell.edu/pub/elgb/astorb.html))
should be supported fairly well.

The creation and maintenance of this package has nothing to do with the Minor
Planet Center, the Lowell Observatory, or NASA.  It was written for [LaRouche
PAC](http://larouchepac.com/), and the LaRouche Policy Institute.


# Reading Examples

## Minor Planet Center's Databases

```haskell
import Data.Binary              (encode)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LCh8
import Codec.Compression.GZip   (decompress)

import qualified Data.ASCII.MinorPlanetCenter.Obs as MPCObs



-- Read all records from a database, and print ther Haskell representations,
-- followed by their database representations:
main = do
        --bs <- LBS.readFile "NumObs.txt.gz"
        bs <- LBS.readFile "UnnObs.txt.gz"
        mapM_ (\r -> do putStrLn $ show r
                        LCh8.putStrLn $ encode r)
              (MPCObs.getMayRecs $ decompress bs)
```


## Lowell Observatory's Database

```haskell
import Data.Binary              (encode)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LCh8
import Codec.Compression.GZip   (decompress)

import qualified Data.ASCII.LowellObservatory.AstOrb as AstOrb



-- Read all records from the 'astorb.dat' database, and print ther Haskell
-- representations, followed by their database representations:
main = do
        bs <- LBS.readFile "astorb.dat.gz"
        mapM_ (\r -> do putStrLn $ show r
                        LCh8.putStrLn $ encode r)
              (AstOrb.getRecs $ decompress bs)
```
