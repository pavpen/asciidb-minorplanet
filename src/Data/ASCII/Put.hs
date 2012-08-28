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

putJulianYYYYMMDD day = putByteString $ Ch8.pack $ printf "%04d%02d%02d" y m d
  where (y, m, d) = toJulian day
