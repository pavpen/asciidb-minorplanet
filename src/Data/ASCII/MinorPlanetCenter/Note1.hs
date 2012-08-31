-----------------------------------------------------------------------------
-- |
-- Module      : Data.ASCII.MinorPlanetCenter.Note1
-- Copyright   : 2012 Pavel M. Penev
-- License     : GPL
--
-- Maintainer  : Pavel Penev <pavpen@gmail.com>
-- Stability   : experimental
--
-- Names for the codes used in the Note 1 field of minor planet observation
-- records as found in the Minor Planet Center's Observation Archive database.
-- (See <http://www.minorplanetcenter.net/iau/ECS/MPCAT-OBS/MPCAT-OBS.html>.)
--
-- The definitions are from:
--
-- 	<http://www.minorplanetcenter.net/iau/info/ObsNote.html>
--
-- The creation and maintenance of this module has nothing to do with the Minor
-- Planet Center, or NASA.  It was written for LaRouche PAC
-- <http://larouchepac.com/>, and the LaRouche Policy Institute.
--
-----------------------------------------------------------------------------

module Data.ASCII.MinorPlanetCenter.Note1 where

earlierApproxPosInferior = 'A'
senseOfMotionAmbiguous = 'a'
brightSkyOrDarkPlate = 'B'
badSeeing = 'b'
crowdedStarField = 'c'
declinationUncertian = 'D'
diffuseImage = 'd'
nearEdgeOfPlate = 'E'
faintImage = 'F'
emulsionOrPlateFlaw = 'f'
poorGuiding = 'G'
noGuiding = 'g'
handMeasurementOfCCD = 'H'
involvedWithStar = 'I'
inkdotMeasured = 'i'
j2000Reduction = 'J'
stackedImage = 'K'
stareModeObsByScanning = 'k'
measurementDifficult = 'M'
objectMotionTracked = 'm'
nearPlateEdgeUncertain = 'N'
outOfFocus = 'O'
plateMeasuredIn1Dir = 'o'
posUncertian = 'P'
poorImage = 'p'
rightAscUncertain = 'R'
poorRfcStarDistrib = 'r'
poorSky = 'S'
streakedImage = 's'
timeUncertain = 'T'
trailedImage = 't'
uncertainImage = 'U'
unconfirmedImage = 'u'
veryFaintImage = 'V'
weakImage = 'W'
weakSolution = 'w'

-- Not to be used in new observations:
earlierPosCorrection = 'C'
qualityObs = 'Q'
replacedEarlierObs = 'X'
