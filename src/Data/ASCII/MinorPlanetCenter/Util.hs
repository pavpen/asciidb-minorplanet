{-# LANGUAGE RecordWildCards #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Data.ASCII.MinorPlanetCenter.Util
-- Copyright   : 2012 Pavel M. Penev
-- License     : GPL
--
-- Maintainer  : Pavel Penev <pavpen@gmail.com>
-- Stability   : experimental
--
-- Utility functions to read and write fields of minor planet observation
-- records as found in the Minor Planet Center's Observation Archive database.
-- (See <http://www.minorplanetcenter.net/iau/ECS/MPCAT-OBS/MPCAT-OBS.html>.)
--
-- Some descriptions of the formats are:
--
-- 	* <http://www.minorplanetcenter.net/iau/info/OpticalObs.html>
--
-- The creation and maintenance of this module has nothing to do with the Minor
-- Planet Center, or NASA.  It was written for LaRouche PAC
-- <http://larouchepac.com/>, and the LaRouche Policy Institute.
--
-----------------------------------------------------------------------------

module Data.ASCII.MinorPlanetCenter.Util where

import Data.Binary.Get		(getBytes, getWord8)
import Data.Binary.Put		(Put (..), putByteString, putWord8)
import qualified Data.ByteString.Char8 as Ch8
import Data.Time		(UTCTime (..), fromGregorian, toGregorian,
				 picosecondsToDiffTime)
import Text.Printf		(printf)
import Safe			(readMay)


-- | Read time ('date of observation') as specified by the Minor Planet Center.
--   (I.e. "YYYY MM DD.dddddd".  See
--   <http://www.minorplanetcenter.net/iau/info/OpticalObs.html>.)
getTime = do
	timeYBs <- getBytes 4 -- The 'YYYY'.
	_ <- getWord8
	timeMBs <- getBytes 2 -- The 'MM'
	_ <- getWord8
	timeDBs <- getBytes 9 -- The 'DD.dddddd'
	let timeD = read $ Ch8.unpack timeDBs
	let timeDs = truncate timeD
	return $ UTCTime { utctDay = fromGregorian (read $ Ch8.unpack timeYBs)
						   (read $ Ch8.unpack timeMBs)
						   timeDs
		      	 , utctDayTime = picosecondsToDiffTime $ round $
			   86400000000000000*(timeD - (fromIntegral timeDs)) }


-- | Write time ('date of observation') as specified by the Minor Planet
--   Center.  (I.e. "YYYY MM DD.dddddd".  See
--   <http://www.minorplanetcenter.net/iau/info/OpticalObs.html>.)
putTime (UTCTime {..}) = putByteString $ Ch8.pack $
	printf "%04d %02d %09.6f" year month ((fromIntegral day) + sec / 86400)
  where (year, month, day) = toGregorian utctDay
	sec = (fromRational $ toRational utctDayTime)::Double


-- | Read the right ascention in seconds ('Observed RA (J2000.0)') as specified
--   by the Minor Planet Center.  [I.e. "HH MM SS.ddd" (or "HH MM.mmm", or
--   "HH.hhh").  See
--   <http://www.minorplanetcenter.net/iau/info/OpticalObs.html>.]
getRightAscSec = do
	bs <- getBytes 12	-- The 'HH MM SS.ddd'
	let str = Ch8.unpack bs
	return $ procsWs $ map read (words str)
  where procsWs :: [Double] -> Double
  	procsWs [h,m,s] = 3600 * h + 60 * m + s
  	procsWs [h,m]   = 3600 * h + 60 * m
	procsWs [h]	= 3600 * h


-- | Like 'getRightAscSec', but wrapped in a Maybe to indicate parsing success
--   and failure.
mayGetRightAscSec = do
	bs <- getBytes 12	-- The 'HH MM SS.ddd'
	let str = Ch8.unpack bs
	return $ procsWs $ map readMay (words str)
  where procsWs :: [Maybe Double] -> Maybe Double
  	procsWs [Just h, Just m, Just s] = Just $ 3600 * h + 60 * m + s
  	procsWs [Just h, Just m]	 = Just $ 3600 * h + 60 * m
	procsWs [Just h]		 = Just $ 3600 * h
	procsWs _			 = Nothing


-- | Write the right ascention ('Observed RA (J2000.0)') given in seconds as
--   specified by the Minor Planet Center.  [I.e. "HH MM SS.ddd" (or "HH
--   MM.mmm", or "HH.hhh").  See
--   <http://www.minorplanetcenter.net/iau/info/OpticalObs.html>.]
--putRightAscSec :: Double -> Put
putRightAscSec sec = putByteString $ Ch8.pack $
	printf "%02d %02d %06.3f" hours min s
  where hours = (truncate $ sec / 3600)::Int
  	min = truncate $ (sec - (fromIntegral $ 3600 * hours)) / 60
	s = sec - (fromIntegral $ 3600 * hours + 60 * min)


-- | Read the declination angle ('Observed Decl. (J2000.0)') as specified by
--   the Minor Planet Center.  [I.e. "sDD MM SS.dd" (or "sDD MM.mm", or
--   "sDD.dd"), "s" being the sign.  See
--   <http://www.minorplanetcenter.net/iau/info/OpticalObs.html>.]
getDeclRad = do
	bs <- getBytes 12
	let str = dropWhile (== '+') $ Ch8.unpack bs
	return $ procsWs $ map read (words str)
  where procsWs :: [Double] -> Double
  	procsWs [d,m,s] = let sgn = sameSgn d
			  in (d + (sgn m) / 60 + (sgn s) / 3600) * pi / 180
  	procsWs [d,m]   = (d + (sameSgn d m) / 60) * pi / 180
	procsWs [d]	= d * pi / 180
	sameSgn x y | x < 0     = -y
	            | otherwise = y


-- | Like 'getDeclRad', but wrapped in a Maybe to indicate parsing success and
--   failure.
mayGetDeclRad = do
	bs <- getBytes 12
	let str = dropWhile (== '+') $ Ch8.unpack bs
	return $ procsWs $ map readMay (words str)
  where procsWs :: [Maybe Double] -> Maybe Double
  	procsWs [Just d, Just m, Just s] =
		let sgn = sameSgn d
		in Just $ (d + (sgn m) / 60 + (sgn s) / 3600) * pi / 180
  	procsWs [Just d, Just m] = Just $ (d + (sameSgn d m) / 60) * pi / 180
	procsWs [Just d]	 = Just $ d * pi / 180
	procsWs _		 = Nothing
	sameSgn x y | x < 0     = -y
	            | otherwise = y


-- | Write the declination angle ('Observed Decl. (J2000.0)') as specified by
--   the Minor Planet Center.  [I.e. "sDD MM SS.dd" (or "sDD MM.mm", or
--   "sDD.dd"), "s" being the sign.  See
--   <http://www.minorplanetcenter.net/iau/info/OpticalObs.html>.]
putDeclRad :: Double -> Put
putDeclRad angRad = do
	putWord8 sgnB
	putWord8 $ 48 + (fromIntegral $ deg `div` 10)
	putWord8 $ 48 + (fromIntegral $ deg `mod` 10) -- "DD"
	putWord8 32 -- ' '
	putWord8 $ 48 + (fromIntegral $ min `div` 10)
	putWord8 $ 48 + (fromIntegral $ min `mod` 10) -- "MM"
	putWord8 32 -- ' '
	if sec > 0 || dd > 0
	  then do putWord8 $ 48 + (fromIntegral $ sec `div` 10)
		  putWord8 $ 48 + (fromIntegral $ sec `mod` 10) -- "SS"
		  if dd > 0
		    then do putWord8 46 -- '.'
			    putWord8 $ 48 + (fromIntegral $ dd `div` 10)
			    putWord8 $ 48 + (fromIntegral $ dd `mod` 10) -- "dd"
		    else do mapM_ putWord8 [32, 32, 32]
	  else  mapM_ putWord8 [32, 32, 32, 32, 32]
  where angDeg = angRad * 180 / pi
        sgnB | angRad < 0 = 45 -- '-'
	     | otherwise  = 43 -- '+'
        angDeciSec = ( round $ abs $ angDeg * 360000 ) :: Int
	dd = angDeciSec `mod` 100
	remSec = angDeciSec `div` 100
	sec = remSec `mod` 60
	remMin = remSec `div` 60
	min = remMin `mod` 60
	deg = remMin `div` 60
