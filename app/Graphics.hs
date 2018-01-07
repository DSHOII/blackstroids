-- Module that brings the game to the screen.

module Graphics
    ( animate
    ) where

-- Packages
import Control.Applicative
import Control.Concurrent
import Control.Monad
import Data.Colour.SRGB (toSRGB24, RGB(..))
import Data.Text (Text)
import Data.StateVar (($=))
import FRP.Yampa
import Linear (V2(..), V4(..))
import Linear.Affine (Point(..))
import qualified SDL
import qualified Data.Vector.Storable as Vector

-- Modules
import ShapesAndTypes

animate :: Text -> Int -> Int -> SF GameInput GameOutput -> IO ()
animate windowTitle windowWidth windowHeight sf = do
  SDL.initialize [SDL.InitVideo]
  gameWindow <- SDL.createWindow windowTitle configuration
  SDL.showWindow gameWindow

  renderer <- SDL.createRenderer gameWindow (-1) SDL.defaultRenderer
  SDL.rendererDrawColor renderer $= V4 maxBound maxBound maxBound maxBound

  lastInteraction <- newMVar =<< SDL.time

  let senseInput _canBlock = do
        currentTime <- SDL.time
        dt <- (currentTime -) <$> swapMVar lastInteraction currentTime
        mEvent <- SDL.pollEvent
        return (dt, Event . SDL.eventPayload <$> mEvent)

      renderOutput changed (obj, shouldExit) = do
        when changed $ do
          renderObject renderer windowHeight obj
          SDL.present renderer
        return shouldExit

  reactimate (return NoEvent) senseInput renderOutput sf

  SDL.destroyRenderer renderer
  SDL.destroyWindow gameWindow
  SDL.quit

  where configuration = SDL.defaultWindow
          { SDL.windowInitialSize = V2 (fromIntegral windowWidth)
                                      (fromIntegral windowHeight) }


renderObject :: SDL.Renderer -> Int -> Object -> IO ()
renderObject renderer windowHeight object = setRenderAttributes >> renderShape
  where setRenderAttributes = do
          let (RGB r g b) = toSRGB24 $ objectColour object
          SDL.rendererDrawColor renderer $= V4 r g b maxBound
        renderShape = case objectType object of
          CubeShape x y -> SDL.fillRect renderer $ Just $
                               SDL.Rectangle (P (V2 (toEnum $ floor px)
                                                    (toEnum $ windowHeight - floor py)))
                                                (V2 (toEnum x) (toEnum y))
          GameScene objects -> do
            SDL.clear renderer
            mapM_ (renderObject renderer windowHeight) objects
        (px, py) = objectPosition object

