-- Game constants

module Constants
       ( cubeWidth
       , cubeHeight
       , cubeColour
       , backgroundColour
       , gameWindowHeight
       , gameWindowWidth
       , thrusterPower
       , gravity
       ) where


-- Modules
import ShapesAndTypes

cubeWidth, cubeHeight :: Double
cubeWidth  = 50
cubeHeight = 50

cubeColour, backgroundColour :: Colour Double
cubeColour       = sRGB24 0xFF 0xFF 0xFF
backgroundColour = sRGB24 0x00 0x00 0x00

gameWindowHeight, gameWindowWidth :: Double
gameWindowHeight = 700
gameWindowWidth  = 700

thrusterPower :: Double
thrusterPower = 100

gravity :: Double
gravity = 200
