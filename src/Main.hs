{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}
module Main (main) where

import Config
import Control.Arrow
import Data.Array
import Data.List
import Game.Input
import Game.Engine
import Game.Logic
import Game.ResourceLoader
import Graphics.Rendering.OpenGL.Monad as GL
import System.IO (stderr)
import System.Log.Formatter
import System.Log.Handler as H
import System.Log.Handler.Simple
import System.Log.Logger as L
import UI.Colors
import UI.Render
import Util.Defs
import Util.HashString

-- | Initializes all the loggers' states to what was defined in the config file.
configLogger :: IO ()
configLogger = do root <- getRootLogger

                  let formatter' = simpleLogFormatter Config.logFormat

                  consoleOutput <- streamHandler stderr Config.logLevel

                  let log' = foldl' (flip ($)) root
                        [ L.setLevel Config.logLevel
                        , setHandlers $ map (`H.setFormatter` formatter')
                              [ consoleOutput ]
                        ]

                  -- Apply the changes to the global logger.
                  saveGlobalLogger log'

                  let addLogLevel (l, prio) = updateGlobalLogger l $
                                                  L.setLevel prio

                  -- Set up all our custom logger levels.
                  mapM_ addLogLevel Config.customLogLevels

data GameState = GameState { rectPos :: Coord
                           , rectRot :: Double -- rotation of the rectangle, in radians.
                           , board :: Board
                           }

-- Get the filename of the texture to load for this piece.
fileString :: Game.Logic.Color -> Piece -> String
fileString c p = "piece-" ++ (colorString c) ++ "-" ++ (pieceString p) ++ ".png"
    where colorString White = "w"
          colorString Black = "b"

          pieceString Pawn = "p"
          pieceString Rook = "r"
          pieceString Knight = "n"
          pieceString Bishop = "b"
          pieceString Queen = "q"
          pieceString King = "k"

chessBoard :: Loaders -> Board -> Renderer
chessBoard l gameBoard = let renderBoard = [ tileRender (x,y) | x <- [0..7], y <- [0..7] ]
                      in defaultRenderer { children = renderBoard, rendDims = (dx*8, dy*8) }
    where
        w, b :: Renderer
        w = textureRenderer l [hashed|"chess-square-w.png"|]
        b = textureRenderer l [hashed|"chess-square-b.png"|]

        idx2pos :: Coord -> Coord
        idx2pos (x, y) = (dx*x, dy*y)

        tileRender :: Coord -> Renderer
        tileRender p@(x, y) = checkerRender p `atIndex` p `withChildren` (pieceRender $ gameBoard!(toEnum $ x + 65, y + 1))
            where checkerRender (x, y) |     evenx &&     eveny = b
                                       | not evenx && not eveny = b
                                       | otherwise              = w
                  evenx = even x
                  eveny = even y
                  pieceRender Nothing = []
                  pieceRender (Just (c, p)) = [(textureRenderer l $ toHashString $ fileString c p)
                                                { pos = Right (HCenterAlign 0, VCenterAlign 0) }]

        withPosition :: Renderer -> Coord -> Renderer
        withPosition r c = r { pos = Left c }

        atIndex :: Renderer -> Coord -> Renderer
        atIndex r = withPosition r . idx2pos

        withChildren :: Renderer -> [Renderer] -> Renderer
        withChildren r c = r { children = c }

        (dx, dy) = rendDims w

display :: GameState -> Dimensions -> Loaders -> GL ()
display gs dims ls = let rect = (rectangleRenderer 600 600 red)
                                    { pos = Left . (subtract 300 *** subtract 300) $ rectPos gs
                                    , children = [ boardRender ]
                                    }
                         boardRender = (chessBoard ls $ board gs)
                                        { pos = Right ( HCenterAlign 0
                                                      , VCenterAlign 0
                                                      )
                                        , rotation = rectRot gs
                                        }
                      in updateWindow dims rect

-- | Solves for the new position of the rectangle, using the mouse as movement.
solveNewPos :: Coord -> InputState -> Coord
solveNewPos _ is = mousePos is

solveNewRot :: Double -> InputState -> Double
solveNewRot r is = r + v * fromIntegral
                        ((fromEnum $ testKeys is [ LeftButton  ])
                       - (fromEnum $ testKeys is [ RightButton ]))
    where
        v = 0.05 -- velocity

-- | We don't do anything... for now.
update :: GameState -> Double -> InputState -> IO (GameState, [ResourceRequest])
update gs _ is = return ( gs { rectPos = solveNewPos (rectPos gs) is,
                               rectRot = solveNewRot (rectRot gs) is
                             }
                        , [ Loaded [hashed|"yellow-dot.png"|]
                          , Loaded [hashed|"chess-square-w.png"|]
                          , Loaded [hashed|"chess-square-b.png"|]
                          ]
                        ++ map (Loaded . toHashString) [fileString c p | c <- [White, Black], p <- [Pawn .. King]]
                        )

initState :: GameState
initState = GameState (100, 100) 0 initBoard

-- Call initialization routines. Register callback function to display
-- graphics. Enter main loop and process events.
main :: IO ()
main = do configLogger
          runGame Config.windowTitle initState display update
