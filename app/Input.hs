-- Module that defines how to handle input. Only keyboard input is handled.

module Input
    ( KeyInput
    , parseGameInput
    , checkKey
    , quitSignal
    , module SDL.Input.Keyboard.Codes
    ) where

-- Libraries
import Data.Maybe
import FRP.Yampa
import SDL.Input.Keyboard.Codes
import qualified SDL

-- Modules
import ShapesAndTypes

-- Detecting key presses.
keyEvent :: SF KeyInput (Event SDL.Scancode)
keyEvent = keyPressed ^>> edgeJust

-- Check if key is one that influences the game.
checkKey :: SDL.Scancode -> SF KeyInput (Event SDL.Scancode)
checkKey code = keyEvent >>^ filterE (code ==) >>^ tagWith code

-- SDL quit event to signal function.
quitSignal :: SF KeyInput (Event ())
quitSignal = arr quitEvent >>> edge

-- Key input as first class values with the pressed key and boolean for quit
-- signal.
data KeyInput = KeyInput
    { keyPressed :: Maybe SDL.Scancode
    , quitEvent       :: Bool
    }

-- Initialize default key input values.
initKeyInput :: KeyInput
initKeyInput = KeyInput { keyPressed = Nothing
                        , quitEvent    = False
                        }

-- Accumulating and parsing events
parseGameInput :: SF GameInput KeyInput
parseGameInput = accumHoldBy nextGameInput initKeyInput


nextGameInput :: KeyInput -> SDL.EventPayload -> KeyInput
nextGameInput input SDL.QuitEvent = input { quitEvent = True }
nextGameInput input (SDL.KeyboardEvent event)
    | SDL.keyboardEventKeyMotion event == SDL.Pressed
      = input { keyPressed = Just $ SDL.keysymScancode
                                  $ SDL.keyboardEventKeysym event }
    | SDL.keyboardEventKeyMotion event == SDL.Released
      = input { keyPressed = Nothing }
nextGameInput input _ = input
