module Data.AngleUtil where

import Data.Angle



class UtilAngle a where
  angDegrees :: (Floating x) => a x -> x
  angRadians :: (Floating x) => a x -> x


instance UtilAngle Radians where
  angDegrees (Radians x) = x * 180 / pi
  angRadians (Radians x) = x


instance UtilAngle Degrees where
  angDegrees (Degrees x) = x
  angRadians (Degrees x) = x * pi / 180
