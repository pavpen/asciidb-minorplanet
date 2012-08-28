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

import Numeric			(readFloat, readSigned)
import Data.Attoparsec.ByteString.Char8
				(isSpace_w8)
import Data.Binary.Get		(getBytes, getWord8)
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
	return $ fst $ head $ readSigned readFloat $ Ch8.unpack fieldBs

mayGetFloatWithWidth width = do
	fieldBs <- getBytes width
	return $ procsParseRes $ readSigned readFloat $ Ch8.unpack fieldBs
  where procsParseRes [] = Nothing
  	procsParseRes ((res, _):_) = Just res

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
