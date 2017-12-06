module Screen(
    windowResolution,
    middleScreen,
    width,
    height
) where

import Graphics.UI.Fungen
import Graphics.Rendering.OpenGL (GLdouble)
import Textures
import Types

windowResolution :: (Int, Int)
windowResolution = (800, 800)

width :: GLdouble
width = fromIntegral (fst windowResolution) :: GLdouble
--width = fst windowResolution

height :: GLdouble
height = fromIntegral (snd windowResolution) :: GLdouble
--height = snd windowResolution

middleScreen :: (Double, Double)
middleScreen = (fromIntegral ((fst windowResolution) `div` 2), fromIntegral ((snd windowResolution) `div` 2))