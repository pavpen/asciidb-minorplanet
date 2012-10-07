{-# LANGUAGE NoMonomorphismRestriction #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Data.ASCII.Get
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

module Data.ASCII.Get where

import Numeric			(readFloat)
import Data.Attoparsec.ByteString.Char8
				(isSpace_w8)
import Data.Binary.Get		(Get (..), getBytes, getWord8, skip)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as Ch8
import Data.Char		(isSpace)
import Data.Time.Calendar.Julian
				(fromJulian)
import Safe			(readMay)



lTrimCh8 = Ch8.dropWhile isSpace

rTrimCh8 = fst . (Ch8.spanEnd isSpace)

trimCh8 = rTrimCh8 . lTrimCh8


lTrimBS = BS.dropWhile isSpace_w8

rTrimBS = fst . (BS.spanEnd isSpace_w8)

trimBS = rTrimBS . lTrimBS


-- | Reads a /signed/ 'Real' value, given a reader for an unsigned value, where
--   the sign can be a '+' in addition to a '-'.
--   (Derived from
--   'http://hackage.haskell.org/packages/archive/base/4.6.0.0/doc/html/src/Numeric.html#readSigned').
readSignedP :: (Real a) => ReadS a -> ReadS a
readSignedP readPos = readParen False read'
                     where read' r  = read'' r ++
                                      (do
                                        ("-",s) <- lex r
                                        (x,t)   <- read''' s
                                        return (-x,t))
                           read'' r  = read''' r ++
                                      (do
                                        ("+",s) <- lex r
                                        read''' s)
                           read''' r = do
                               (str,s) <- lex r
                               (n,"")  <- readPos str
                               return (n,s)



readWithWidth width = do
	fieldBs <- getBytes width
	return $ read $ Ch8.unpack fieldBs

mayReadWithWidth width = do
	fieldBs <- getBytes width
	return $ readMay $ Ch8.unpack fieldBs

readWithWidthDeflt width deflt = do
	mayRes <- mayReadWithWidth width
	return $ maybe deflt id mayRes

getFloatWithWidth width = do
	fieldBs <- getBytes width
	return $ fst $ head $ readSignedP readFloat $ Ch8.unpack fieldBs

mayGetFloatWithWidth width = do
	fieldBs <- getBytes width
	return $ procsParseRes $ readSignedP readFloat $ Ch8.unpack fieldBs
  where procsParseRes [] = Nothing
  	procsParseRes ((res, _):_) = Just res

getDecFracWithWidth' startNum startDgtFactor width = do
	fieldBs <- getBytes width
	return $ fst $ Ch8.foldl procsDgt (startNum, startDgtFactor) fieldBs
  where ch2Dgt ' ' = 0
  	ch2Dgt ch  = fromIntegral $ (fromEnum ch) - 48
	procsDgt (accum, dgtFactor) ch =
		(accum + dgtFactor * (ch2Dgt ch), dgtFactor / 10)

getFloatImplFPWithWidth unitsWidth fracWidth = do
	units <- getFloatWithWidth unitsWidth
	getDecFracWithWidth' units 0.1 fracWidth

mayGetFloatImplFPWithWidth unitsWidth fracWidth = do
	units <- mayGetFloatWithWidth unitsWidth
	maybe (do skip fracWidth
		  return Nothing)
	      (\u -> fmap Just $ getDecFracWithWidth' u 0.1 fracWidth)
	      units

alNumW8Num w8 | w8 >= 48 && w8 <= 57 = w8 - 48	-- '0' to '9'
	      | w8 >= 65 && w8 <= 90 = w8 - 55	-- 'A' to 'Z'
	      | otherwise = error $ "Invalid alNum word8: " ++ (show w8)

alNumCh2Num ch = alNumW8Num $ fromEnum ch

getAlNumWithWidthDeflt width deflt = do
	fieldBs <- getBytes width
	return $ res $ trimCh8 fieldBs
  where res str | Ch8.null str = deflt
  	        | otherwise = Ch8.foldl' procsCh 0 str
	procsCh num ch = 10 * num + (alNumCh2Num ch)

getEnumWord8 = do
	w8 <- getWord8
	return $ toEnum $ fromIntegral w8

getStrWithWidth width = do
	fieldBs <- getBytes width
	return $ Ch8.unpack fieldBs

getTrimmedStrWithWidth width = do
	fieldBs <- getBytes width
	return $ Ch8.unpack $ trimBS fieldBs

getJulianYYYYMMDD = do
	y <- readWithWidth 4
	m <- readWithWidth 2
	d <- readWithWidth 2
	return $ fromJulian y m d
