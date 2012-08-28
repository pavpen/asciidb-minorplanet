import Data.Binary		(decode, encode)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LCh8
import Codec.Compression.GZip	(decompress)

import qualified Data.ASCII.MinorPlanetCenter.Obs as MPCObs
import qualified Data.ASCII.MinorPlanetCenter.ProvisionalDesignations as PD



main = do
	putStrLn $ PD.showPacked (PD.MinorPlanetId 1995 5 False 100)
	putStrLn $ PD.showPacked (PD.SurveyT1Id 1995)
	putStrLn $ show $ PD.readLong "1995 SA2"
	putStrLn $ show $ PD.readLong "2040 T-1"
	putStrLn $ show $ PD.readPacked "J95S02A"
	putStrLn $ show $ PD.readPacked "T1S2040"
	--bs <- LBS.readFile "NumObs.txt.gz"
	bs <- LBS.readFile "UnnObs.txt.gz"
	putStrLn $ show $ ((decode $ decompress bs)::MPCObs.Rec)
	putStrLn ""
	mapM_ (\r -> do putStrLn $ show r
			LCh8.putStrLn $ encode r)
	      (MPCObs.getMayRecs $ decompress bs)
