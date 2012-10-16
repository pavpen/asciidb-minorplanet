-----------------------------------------------------------------------------
-- |
-- Module      : Data.ASCII.Put
-- Copyright   : 2012 Pavel M. Penev
-- License     : GPL
--
-- Maintainer  : Pavel Penev <pavpen@gmail.com>
-- Stability   : experimental
--
-- The creation and maintenance of this module was written for LaRouche PAC
-- <http://larouchepac.com/>, and the LaRouche Policy Institute.
--
-----------------------------------------------------------------------------

module Data.ASCII.Put where

import Data.Binary.Put		(putByteString, putWord8)
import qualified Data.ByteString.Char8 as Ch8
import Data.Time.Calendar.Julian
				(toJulian)
import Control.Monad		(foldM_)
import Text.Printf		(printf)


putFloatWithWidth width num
  | unitsStrLen < width - 1 = do
  	putByteString $ Ch8.pack $ unitsStr
  	putWord8 46 -- '.'
  	putByteString $ Ch8.pack $ replicate (decLen - (length decNumStr)) '0'
	putByteString $ Ch8.pack $ decNumStr
  | unitsStrLen < width = do
  	putWord8 32 -- ' '
	putByteString $ Ch8.pack $ unitsStr
  | otherwise = putByteString $ Ch8.pack $ unitsStr
  where units :: Integer
  	units = truncate num
	unitsStr | num < 0   = '-':(show $ abs units)
		 | otherwise = show $ abs units
	unitsStrLen = length unitsStr
	decLen = width - 1 - unitsStrLen
	decPart = abs $ num - (fromIntegral units)
	decNum :: Integer
	decNum = round $ decPart * 10**(fromIntegral decLen)
	decNumStr = show decNum

num2AlNumW8 n | n >= 0 && n <= 9 = n + 48	-- '0' to '9'
	      | n >= 10 && n <= 35 = n + 55	-- 'A' to 'Z'
	      | otherwise = error $ "AlNum digit out of range: " ++ (show n)

num2AlNumCh :: Int -> Char
num2AlNumCh = toEnum . num2AlNumW8

putAlNumWithWidth width num | width < 1 = return ()
			    | otherwise = do	
	let dgtFactor = 10 ^ (width - 1)
	let (dgt, remNum) = num `divMod` dgtFactor
	putWord8 $ fromIntegral $ num2AlNumW8 dgt
  	putRest remNum (dgtFactor `div` 10) (width - 1)
  where putRest _ _ 0		    = return ()
  	putRest num dgtFactor width = do
		let (dgt, remNum) = num `divMod` dgtFactor
		putWord8 $ fromIntegral $ num2AlNumW8 dgt
		putRest remNum (dgtFactor `div` 10) (width - 1)

putJulianYYYYMMDD day = putByteString $ Ch8.pack $ printf "%04d%02d%02d" y m d
  where (y, m, d) = toJulian day
