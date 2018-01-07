-- This module contains the shapes the renderer can render and helping functions.

module ShapesAndTypes
       ( GameInput
       , GameOutput
       , GameObject(..)
       , Object(..)
       , def
       , gameScene
       , renderCube
       , position
       , colour
       , Colour(..)
       , sRGB24
       , (&)
       ) where

-- Packages
import Data.Default
import Data.Colour
import Data.Colour.SRGB
import Data.Colour.Names
import qualified SDL
import FRP.Yampa


type GameInput = Event SDL.EventPayload

type GameOutput = (Object, Bool)


data GameObject = CubeShape Int Int
                | GameScene [Object]
                deriving (Show, Eq)


data Object = Object { objectType    :: GameObject
                     , objectPosition :: (Double, Double)
                     , objectColour   :: Colour Double
                     } deriving (Show, Eq)

instance Default Object where
  def = Object { objectType   = error "Object is not defined"
               , objectPosition = (0, 0)
               , objectColour   = white
               }

gameScene :: [Object] -> Object
gameScene objects = def { objectType = GameScene objects, objectColour = black}

renderCube :: Double -> Double -> Object
renderCube x y = def { objectType = CubeShape (round x) (round y) }

type AttributeSetter = Object -> Object

position :: (Double, Double) -> AttributeSetter
position position object = object { objectPosition = position }

colour :: Colour Double -> AttributeSetter
colour colour object = object { objectColour = colour }

(&) :: Object -> AttributeSetter -> Object
(&) = flip ($)

