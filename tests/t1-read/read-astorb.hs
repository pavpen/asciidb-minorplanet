{-# LANGUAGE RecordWildCards #-}

import Data.Binary		(decode, encode)
import Data.Binary.Put		(runPut)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LCh8
import Data.Time.Calendar	(fromGregorian)
import Codec.Compression.GZip	(decompress)

import qualified Data.ASCII.LowellObservatory.AstOrb as AstOrb
import qualified Data.ASCII.MinorPlanetCenter.ProvisionalDesignations as PD



tRec = AstOrb.Rec
		{ objNumber = 1
		, designation = "Ceres"
		, orbitComputer = "Severinus"
		, magnitude = 1
		, slopeParam = 2
		, colorIndex = Just 3
		, irasDiam = Just 4
		, taxon = "spec"
		, orbitCrossings = toEnum 0
		, orbitUncertainties = toEnum 0
		, surveys = toEnum 0
		, observations = toEnum 0
		, discoverers = toEnum 0
		, rank = 5
		, obsDays = 14
		, usedObsCnt = 3
		, osculationEpoch = fromGregorian 2012 8 28
		, meanAnomRad = 6
		, argPerihRad = 7
		, lngAscNodeRad = 8
		, inclRad = 9
		, ecc = 10
		, semiMajAAU = 11
		, orbitCompDate = fromGregorian 1828 08 27
		, ceuRad = 12
		, ceuChangeRadPDay = 13
		, ceuDate = fromGregorian 2012 8 29
		, peuRad = 14
		, peuDate = fromGregorian 2012 8 30
		, max10yPEURad = 15
		, max10yPEUDate = fromGregorian 2012 9 1
		, peu10yRad = 16
		, peu10yDate = fromGregorian 2012 9 2
		}

main = do
	bs <- LBS.readFile "astorb.dat.gz"
	--putStrLn $ show $ ((decode $ decompress bs)::AstOrbRec)
	putStrLn $ LCh8.unpack $ runPut (AstOrb.putRec tRec)
	putStrLn "                                                                                                   1                                                                                                   2"
	putStrLn "         1         2         3         4         5         6         7         8         9         0         1         2         3         4         5         6         7         8         9         0         1         2         3         4         5         6"
	putStrLn "123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567"
	putStrLn ""
	mapM_ (\r -> do putStrLn $ show r
			LCh8.putStrLn $ encode r)
	      (AstOrb.getRecs $ decompress bs)
