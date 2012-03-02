{-# LANGUAGE QuasiQuotes, OverloadedStrings, BangPatterns #-}
module Main (main) where

import Config
import Control.Arrow
import Control.DeepSeq
import Data.Array
import Data.List
import Game.Input
import Game.Engine
import Game.Logic
import Game.ResourceLoader
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

data GameState = GameState { t0      :: !Double -- the time value of the last frame.
                           , rectPos :: Coord
                           , rectRot :: Double -- rotation of the rectangle, in radians.
                           , board   :: Board
                           , mvSrc   :: Maybe Position -- if the user's selected a piece to move,
                                                      -- they've selected the one here.
                           , turn    :: Game.Logic.Color -- whose turn is it?
                           }

instance NFData GameState where
    rnf gs = rnf (rectPos gs) `seq`
             rnf (rectRot gs) `seq`
             rnf (board   gs) `seq`
             rnf (mvSrc   gs) `seq`
             rnf (turn    gs) `seq`
             ()

-- Get the filename of the texture to load for this piece.
fileString :: Game.Logic.Color -> Piece -> HashString
fileString c p = toHashString $ "piece-" ++ (colorString c) ++ "-" ++ (pieceString p) ++ ".png"
    where colorString White = "w"
          colorString Black = "b"

          pieceString Pawn = "p"
          pieceString Rook = "r"
          pieceString Knight = "n"
          pieceString Bishop = "b"
          pieceString Queen = "q"
          pieceString King = "k"

-- Prevents recomputation of our piece hashstrings.
allPieces :: [HashString]
allPieces = [ fileString c p | c <- [White, Black] , p <- [Pawn .. King] ]

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
        tileRender p@(x, y) = checkerRender `atIndex` p
                                            `withChildren` (pieceRender $ gameBoard!(toEnum $ x + 65, y + 1))
            where checkerRender |     evenx &&     eveny = b
                                | not evenx && not eveny = b
                                | otherwise              = w
                  evenx = even x
                  eveny = even y
                  pieceRender Nothing = []
                  pieceRender (Just (c, pce)) = [(textureRenderer l $ fileString c pce)
                                                    { pos = Right (HCenterAlign 0, VCenterAlign 0) }]

        withPosition :: Renderer -> Coord -> Renderer
        withPosition r c = r { pos = Left c }

        atIndex :: Renderer -> Coord -> Renderer
        atIndex r = withPosition r . idx2pos

        withChildren :: Renderer -> [Renderer] -> Renderer
        withChildren r c = r { children = c }

        (dx, dy) = rendDims w

display :: GameState -> Loaders -> Renderer
display gs ls = let rect = (rectangleRenderer 600 600 red)
                                { pos = Left . (subtract 300 *** subtract 300) $ rectPos gs
                                , children = [ boardRender ]
                                }
                    boardRender = (chessBoard ls $ board gs)
                                { pos = Right ( HCenterAlign 0
                                              , VCenterAlign 0
                                              )
                                , rotation = rectRot gs
                                }
                 in rect

{-
solveNewPos :: Coord -> InputState -> Coord
solveNewPos _ = mousePos
-}

solveNewRot :: Double -> Double -> InputState -> Double
solveNewRot r dt is = r + v*dt * fromIntegral
                                 ((fromEnum $ testKeys is [ KeyChar 'z' ])
                                - (fromEnum $ testKeys is [ KeyChar 'x' ]))
    where
        v = 1 -- velocity

considerMovement :: GameState -> InputState -> Maybe GameState
considerMovement gs is = do tile <- clickCoords
                            case mvSrc gs of
                                Nothing -> select tile
                                Just src -> src `moveTo` tile

    where clickCoords = if testKeys is [ LeftButton ]
                        then let (x, y) = mousePos is
                             in if x >= 144 && x < 800 - 144
                                && y >=  44 && y < 600 -  44
                                 then Just (toEnum $ (x - 144) `div` 64 + 65,
                                            toEnum $ (y -  44) `div` 64 + 1)
                                 else Nothing
                        else Nothing

          select tile = (board gs)!tile >>= \(color, _) -> if turn gs == color
                                                           then Just $ gs { mvSrc = Just tile }
                                                           else Nothing

          moveTo src tile = do gameBoard <- move (board gs) src tile
                               return $ gs { mvSrc = Nothing
                                           , board = gameBoard
                                           , turn = next $ turn gs
                                           }

-- | We don't do anything... for now.
update :: GameState -> Double -> InputState -> IO (GameState, [ResourceRequest], Loaders -> Renderer)
update gs !t is = let gs'  = maybe gs id (considerMovement gs is)
                      gs'' = gs' { t0 = t
                                 --, rectPos = solveNewPos (rectPos gs) is
                                 , rectRot = solveNewRot (rectRot gs) dt is
                                 }
                   in return $!! ( gs''
                                 , [ Loaded [hashed|"chess-square-w.png"|]
                                   , Loaded [hashed|"chess-square-b.png"|]
                                 ] ++ map Loaded allPieces
                                 , display gs''
                                 )
    where
        dt | g0 > 0 = t - g0
           | g0 == 0 = 0 -- only happens on the first update.
           | otherwise = error $ "So apparently, we have a time value less than 0: " ++ show g0
            where
                g0 = t0 gs

initState :: GameState
initState = GameState 0 (400, 300) 0 initBoard Nothing White

-- Call initialization routines. Register callback function to display
-- graphics. Enter main loop and process events.
main :: IO ()
main = do configLogger
          runGame Config.windowTitle initState update
