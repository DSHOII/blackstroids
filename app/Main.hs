{-# LANGUAGE Arrows, OverloadedStrings #-}

-- This is the main module for the very small game (barely a game)
-- Blackstroids. The game implementation is strongly inspired by yampa-cube by
-- Konstantin Zudow:

-- https://github.com/helsinki-frp/yampy-cube

module Main where

-- Libraries
import FRP.Yampa

-- Modules
import Graphics
import ShapesAndTypes
import Input
import Constants

import qualified SDL

-- Main function. Game appearance can be tweaked from Constats module.
main :: IO ()
main = animate "Blackstroids" (round gameWindowWidth) (round gameWindowHeight)
                            (parseGameInput >>> ((game >>^ renderGameObjects)
                                                 &&& handleExit))

-- Data constructors to hold game state and initialisation functions.
data Cube = Cube { cubePosition :: (Double, Double)
                 , cubeVelocity :: Double
                 }

initiateCube :: Cube
initiateCube = Cube (gameWindowWidth / 2, gameWindowHeight - 50) 0

data Game = Game { gameCube :: Cube }

initiateGame :: Game
initiateGame = Game initiateCube


-- Renderer that runs over all gameobjects and renders their shape.
renderGameObjects :: Game -> Object
renderGameObjects (Game (Cube (cubeX,cubeY) _)) = scene & colour backgroundColour
    where scene = gameScene [cube]
          cube = renderCube cubeWidth cubeHeight & position (cubeX, cubeY)
                                                 & colour cubeColour


-- | Signal functions that define the game logic.
game :: SF KeyInput Game
game = switch sf (\_ -> game)
    where sf = proc input -> do
              gameState <- gameSession -< input
              gameOver <- edge -< checkBoundry gameState
              returnA -< (gameState, gameOver)

-- Function that checks if the cube is outside the game window.
checkBoundry :: Game -> Bool
checkBoundry (Game (Cube (_ , cubePositionY) _)) =
  or [ cubePositionY < 0 , cubePositionY > gameWindowHeight ]

-- The continued game if boundry checks succeeds.
gameSession :: SF KeyInput Game
gameSession = proc input -> do
    cube <- cubeThrusters initiateCube -< input
    returnA -< Game cube

-- Signal function that simulates gravity on the cube.
gravityCube :: Cube -> SF a Cube
gravityCube (Cube (x0,y0) v0) = proc _ -> do
    v <- imIntegral v0 -< -gravity
    y <- imIntegral y0 -< v
    returnA -< Cube (x0,y) v

-- Signal function that adds upwards acceleration to the cube on valid
-- input. Called thrusters because the game was supposed to be about spaceships.
cubeThrusters :: Cube -> SF KeyInput Cube
cubeThrusters cube0 = switch sf cont
    where sf = proc input -> do
              cube <- gravityCube cube0 -< ()
              flap <- burnThrusters -< input
              returnA -< (cube, flap `tag` cube)
          cont (Cube (x,y) v) = cubeThrusters (Cube (x,y) (v + thrusterPower))

-- Signal function that checks if input is from the right buttons.
burnThrusters :: SF KeyInput (Event SDL.Scancode)
burnThrusters = proc input -> do
    spacebarTap <- checkKey ScancodeSpace -< input
    leftTap <- checkKey ScancodeLeft -< input
    rightTap <- checkKey ScancodeRight -< input
    upTap <- checkKey ScancodeUp -< input
    downTap <- checkKey ScancodeDown -< input
    returnA -< mergeEvents [spacebarTap, leftTap, rightTap, upTap, downTap]

-- Signal function that handels quit events.
handleExit :: SF KeyInput Bool
handleExit = quitSignal >>^ isEvent
