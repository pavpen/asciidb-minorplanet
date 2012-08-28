{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Data.ASCII.MinorPlanetCenter.ProvisionalDesignations
-- Copyright   : 2012 Pavel M. Penev
-- License     : GPL
--
-- Maintainer  : Pavel Penev <pavpen@gmail.com>
-- Stability   : experimental
--
-- Read and write minor planet provisional designations, as found in the Minor
-- Planet Center's Observation Archive database
-- (<http://www.minorplanetcenter.net/iau/ECS/MPCAT-OBS/MPCAT-OBS.html>.)
--
-- The descriptions of the formats are:
--
-- 	* <http://www.minorplanetcenter.net/iau/info/PackedDes.html>
--
-- 	* <http://www.minorplanetcenter.net/iau/info/OldDesDoc.html>
--
-- The creation and maintenance of this module has nothing to do with the Minor
-- Planet Center, or NASA.  It was written for LaRouche PAC
-- <http://larouchepac.com/>, and the LaRouche Policy Institute.
--
-----------------------------------------------------------------------------

module Data.ASCII.MinorPlanetCenter.ProvisionalDesignations where

import Data.ASCII.Get		(trimBS)
import Data.Binary.Get		(getBytes)
import qualified Data.ByteString.Char8 as Ch8
import Text.Printf		(printf)


-- | Object provisional designation.  See
--   <http://www.minorplanetcenter.net/iau/info/OldDesDoc.html>, and
--   <http://www.minorplanetcenter.net/iau/info/PackedDes.html>.
--
--   'order', and 'month' are 1-based.
data ProvDesign = MinorPlanetId { year :: Int
				, month :: Int
				, monthSecHalf :: Bool
				, order :: Int }
		| SurveyPLId	{ order :: Int } -- Palomar-Leiden (1960)
		| SurveyT1Id	{ order :: Int } -- First Trojan Survey (1971)
		| SurveyT2Id	{ order :: Int } -- Second Trojan Survey (1973)
		| SurveyT3Id	{ order :: Int } -- Third Trojan Survey (1977)
		| CometId	{ year :: Int
				, month :: Int
				, monthSecHalf :: Bool
				, order :: Int
				, status :: Char
				, fragment :: Char }
		deriving (Show)


-- | Calculate the letter corresponding to given number, omitting 'I', as used
--   in half-month letters, and object order letters.  (See
--   <http://www.minorplanetcenter.net/iau/info/OldDesDoc.html>.)
--   
--   'n' is 0-based!
num2Ch :: Int -> Char
num2Ch n | n <= 7    = toEnum $ 65 + n
         | otherwise = toEnum $ 66 + n


-- | Calculate the number corresponding to given letter, omitting 'I', as used
--   in half-month letters, and object order letters.  (See
--   <http://www.minorplanetcenter.net/iau/info/OldDesDoc.html>.)
--   
--   The number is 0-based!
ch2Num :: Char -> Int
ch2Num ch = num $ (fromEnum ch) - 65
  where num n | n <= 7    = n
  	      | otherwise = n - 1


-- | Calculate the Char corresponding to a given number using a base-62 system
--   of the digits (0--9), the uppercase letters (A--Z), and the lowercase
--   letters (a--z), as used to designate the order number in an object's
--   packed provisional designation.  (See
--   <http://www.minorplanetcenter.net/iau/info/PackedDes.html>.)
num2B62Ch n | n < 10    = toEnum $ 48 + n
	    | n < 36    = toEnum $ 65 + (n - 10)
	    | otherwise = toEnum $ 97 + (n - 36)


-- | Calculate the number corresponding to base-62 system Char, as used to
--   designate the order number in an object's packed provisional designation.
--   The base-62 system uses the digits (0--9), the uppercase letters (A--Z),
--   and the lowercase letters (a--z).  (See
--   <http://www.minorplanetcenter.net/iau/info/PackedDes.html>.)
b62Ch2Num ch | chCode < 58 = chCode - 48
	     | chCode < 91 = chCode - 55
	     | otherwise   = chCode - 61
  where chCode = fromEnum ch


-- | Calculate the half-month letter designating a given month (1 = January),
--   and a given half of it. (See
--   <http://www.minorplanetcenter.net/iau/info/OldDesDoc.html>.)
halfMoCh :: Int -> Bool -> Char
halfMoCh mo False = num2Ch $ 2 * (mo - 1)
halfMoCh mo _	  = num2Ch $ 2 * mo - 1


-- | Calculate the month number (1 = January), and whether its the second half
--   of the month for a given half-month letter.  (See
--   <http://www.minorplanetcenter.net/iau/info/OldDesDoc.html>.)
chHalfMo :: Char -> (Int, Bool)
chHalfMo ch = (n `div` 2 + 1, n `mod` 2 > 0)
  where n = ch2Num ch


-- | Calculate the order letter and number for a given minor planet, which was
--   not observed as part of a survey.  (See
--   <http://www.minorplanetcenter.net/iau/info/OldDesDoc.html>.)
orderStr :: Int -> String
orderStr o = (num2Ch ch1Num):remStr
  where ch1Num = (o - 1) `mod` 25
  	remNum = (o - 1) `div` 25
	remStr | remNum <= 0 = ""
	       | otherwise   = show remNum


-- | Calculate the non-packed string designation of an object.  (See
--   <http://www.minorplanetcenter.net/iau/info/OldDesDoc.html>.)
showLong (MinorPlanetId {..}) =
	(show year) ++ " " ++ [halfMoCh month monthSecHalf] ++ (orderStr order)
showLong (SurveyPLId {..}) = (show order) ++ " P-L"
showLong (SurveyT1Id {..}) = (show order) ++ " T-1"
showLong (SurveyT2Id {..}) = (show order) ++ " T-2"
showLong (SurveyT3Id {..}) = (show order) ++ " T-3"


-- | Construct the provisional object designation from a non-packed string
--   designation of the object.  (See
--   <http://www.minorplanetcenter.net/iau/info/OldDesDoc.html>.)
readLong str = procsRemStr $ dropWhile (== ' ') remStrRaw
  where (num, remStrRaw) = head $ ( (reads str) :: [(Int, String)] )
        procsRemStr "P-L" = SurveyPLId { order = num }
	procsRemStr "T-1" = SurveyT1Id { order = num }
	procsRemStr "T-2" = SurveyT2Id { order = num }
	procsRemStr "T-3" = SurveyT3Id { order = num }
	procsRemStr [halfMoCh, ordCh] =
		let (month, monthSecHalf) = chHalfMo halfMoCh
		in MinorPlanetId { year = num, month, monthSecHalf,
				   order = 1 + (ch2Num ordCh) }
	procsRemStr (halfMoCh:ordCh:ordNumStr) =
		let (month, monthSecHalf) = chHalfMo halfMoCh
		in MinorPlanetId { year = num, month, monthSecHalf,
			order = (read ordNumStr) * 25 + (ch2Num ordCh) + 1 }


-- | Calculate the packed string designation of an object.  (See
--   <http://www.minorplanetcenter.net/iau/info/PackedDes.html>.)
showPacked (MinorPlanetId {..}) =
	[toEnum $ 55 + cent, toEnum $ 48 + decd `div` 10,
			     toEnum $ 48 + decd `mod` 10, -- year
	 halfMoCh month monthSecHalf,
	 num2B62Ch $ ordRemNum `div` 10, toEnum $ 48 + ordRemNum `mod` 10,
	 num2Ch ordCh1Num]
  where cent = year `div` 100
        decd = year `mod` 100
	ordCh1Num = (order - 1) `mod` 25
	ordRemNum = (order - 1) `div` 25
showPacked (SurveyPLId {..}) = printf "PLS%04d" order 
showPacked (SurveyT1Id {..}) = printf "T1S%04d" order
showPacked (SurveyT2Id {..}) = printf "T2S%04d" order
showPacked (SurveyT3Id {..}) = printf "T3S%04d" order


-- | Construct the provisional object designation from a packed string
--   designation of the object.  (See
--   <http://www.minorplanetcenter.net/iau/info/OldDesDoc.html>.)
readPacked ('P':'L':'S':ordStr) = SurveyPLId { order = read ordStr }
readPacked ('T':'1':'S':ordStr) = SurveyT1Id { order = read ordStr }
readPacked ('T':'2':'S':ordStr) = SurveyT2Id { order = read ordStr }
readPacked ('T':'3':'S':ordStr) = SurveyT3Id { order = read ordStr }
readPacked [centCh, decdCh1, decdCh2, halfMoCh, ordRemCh1, ordRemCh2, ordCh1] =
	MinorPlanetId {..}
  where year = ((fromEnum centCh) - 55) * 100 +
  	       ((fromEnum decdCh1) - 48) * 10 +
	       ((fromEnum decdCh2) - 48)
	(month, monthSecHalf) = chHalfMo halfMoCh
	order = 1 + (ch2Num ordCh1) + 25 * ( 10 * (b62Ch2Num ordRemCh1) +
					     ((fromEnum ordRemCh2) - 48) )

-- | Read a 7-character packed provisional designation in the Get monad,
--   returning it in a 'Just', or returning 'Nothing', if the field is blank.
--
--   This can be used for reading the 'provisional designation' field from an
--   observations database file.  (See
--   <http://www.minorplanetcenter.net/iau/info/OldDesDoc.html>)
mayGetPacked = do
	fieldBs <- getBytes 7
	let fieldStr = Ch8.unpack $ trimBS $ fieldBs
	case fieldStr of
	  [] -> return Nothing
	  _  -> return $ Just $ readPacked fieldStr
