{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}
module Main (main) where

import Chess()
import Config
import Data.List
import Game.Engine
import Game.ResourceLoader
import Graphics.Rendering.OpenGL.Monad as GL
import System.IO (stderr)
import System.Log.Formatter
import System.Log.Handler as H
import System.Log.Handler.Simple
import System.Log.Logger as L
import UI.Colors
import UI.Render
import Util.HashString

-- | Initializes all the loggers' states to what was defined in the config file.
configLogger :: IO ()
configLogger = do root <- getRootLogger

                  let formatter' = simpleLogFormatter logFormat

                  consoleOutput <- streamHandler stderr logLevel

                  let log' = foldl' (flip ($)) root
                        [ L.setLevel logLevel
                        , setHandlers $ map (`H.setFormatter` formatter')
                              [ consoleOutput ]
                        ]

                  -- Apply the changes to the global logger.
                  saveGlobalLogger log'

                  -- Set up all our custom logger levels.
                  mapM_ (\(logName, prio) -> updateGlobalLogger logName $ L.setLevel prio) customLogLevels

data GameState = GameState { shouldShow :: Bool -- Should the scene be rendered?
                           }

display :: GameState -> Dimensions -> Loaders -> GL ()
display gs dims ls = let (Just tex) = getResource (textureL ls) "yellow-dot.png"
                         rect = (rectangleRenderer 200 200 red)
                                  { hAlign = Just $ HCenterAlign 0
                                  , vAlign = Just $ VCenterAlign 0
                                  , rotation = pi/4
                                  , rotateAround = (100, 100)
                                  , children = [ dot ]
                                  }
                         dot = (textureRenderer tex)
                                  { hAlign = Just $ HCenterAlign 0
                                  , vAlign = Just $ VCenterAlign 0
                                  }
                     in if shouldShow gs then updateWindow dims dot
                                         else return ()

-- | We don't do anything... for now.
update :: GameState -> Double -> IO (GameState, [ResourceRequest])
update _ _ = return (GameState True,
                      [ Loaded [hashed|yellow-dot.png|]
                      ] )

-- | Event handling is currently unimplemented.
onEvent :: GameState -> Event -> GameState
onEvent gs _ = gs

initState :: GameState
initState = GameState False

-- Declare initial window size, position, and display mode (single buffer and
-- RGBA). Open window with "hello" in its title bar. Call initialization
-- routines. Register callback function to display graphics. Enter main loop and
-- process events.
main :: IO ()
main = do configLogger
          runGame "Chess - By B & C" initState display update onEvent
