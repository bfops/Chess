{-# LANGUAGE QuasiQuotes, OverloadedStrings, BangPatterns, CPP #-}
module Main (main) where

import Config
import Control.DeepSeq
import Control.Monad
import Data.Array
import qualified Data.Cycle as C
import qualified Data.HashMap.Strict as M
import Data.HashString
import Data.List
import Data.Maybe
import Game.Engine
import Game.Input
import Game.Logic
import Game.Render
import Game.Render.Colors
import Game.Resource.Loader
import System.IO (stderr)
import System.Log.Formatter
import System.Log.Handler as H
import System.Log.Handler.Simple
import System.Log.Logger as L
import Util.Defs

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
                           , game    :: UniqueGame
                           , mvSrc   :: Maybe Position -- if the user's selected a piece to move,
                                                      -- they've selected the one here.
                           }

instance NFData GameState where
    rnf gs = rnf (rectPos gs) `seq`
             rnf (rectRot gs) `seq`
             rnf (game    gs) `seq`
             rnf (mvSrc   gs) `seq`
             ()

#define DEFPIECE(name) ([hashed|name|], [texRend|name|])

pieceMap :: M.HashMap HashString Renderer
pieceMap = M.fromList [ DEFPIECE("piece-b-b.png")
                      , DEFPIECE("piece-b-k.png")
                      , DEFPIECE("piece-b-n.png")
                      , DEFPIECE("piece-b-p.png")
                      , DEFPIECE("piece-b-q.png")
                      , DEFPIECE("piece-b-r.png")
                      , DEFPIECE("piece-w-b.png")
                      , DEFPIECE("piece-w-k.png")
                      , DEFPIECE("piece-w-n.png")
                      , DEFPIECE("piece-w-p.png")
                      , DEFPIECE("piece-w-q.png")
                      , DEFPIECE("piece-w-r.png")
                      ]

#undef DEFPIECE

-- Get the filename of the texture to load for this piece.
fileString :: Game.Logic.Color -> Piece -> HashString
fileString c p = toHashString $ "piece-" ++ (colorString c) ++ "-" ++ (pieceString p) ++ ".png"
    where colorString White = "w"
          colorString Black = "b"

          pieceString (Pawn _) = "p"
          pieceString (Rook _) = "r"
          pieceString Knight   = "n"
          pieceString Bishop   = "b"
          pieceString Queen    = "q"
          pieceString (King _) = "k"

-- Prevents recomputation of our piece hashstrings.
allPieces :: [HashString]
allPieces = [ fileString c p | c <- [White, Black] , p <- [Pawn False, Rook False, Knight, Bishop, Queen, King False] ]

chessBoard :: Board -> Renderer
chessBoard gameBoard | rendDims w /= rendDims b = error "White and black square textures are not the same size."
                     | otherwise               = let renderBoard = [ tileRender (x,y) | x <- [0..7], y <- [0..7] ]
                                                  in defaultRenderer { children = renderBoard, rendDims = (dx*8, dy*8) }
    where
        w, b :: Renderer
        w = [texRend|"chess-square-w.png"|]
        b = [texRend|"chess-square-b.png"|]

        idx2pos :: Coord -> Coord
        idx2pos (x, y) = (dx*x, dy*y)

        tileRender :: Coord -> Renderer
        tileRender p@(x, y) = checkerRender `atIndex` p
                                            `withChildren` (renderTileContents $ gameBoard!(shift ('A', 1) p))
            where checkerRender |     evenx &&     eveny = b
                                | not evenx && not eveny = b
                                | otherwise              = w
                  evenx = even x
                  eveny = even y
                  renderTileContents = maybe [] renderPiece
                  renderPiece (c, pce) = [(fromJust $ M.lookup (fileString c pce) pieceMap)
                                            { pos = Right (HCenterAlign 0, VCenterAlign 0) }]

        withPosition :: Renderer -> Coord -> Renderer
        withPosition r c = r { pos = Left c }

        atIndex :: Renderer -> Coord -> Renderer
        atIndex r = withPosition r . idx2pos

        withChildren :: Renderer -> [Renderer] -> Renderer
        withChildren r c = r { children = c }

        (dx, dy) = rendDims w

display :: GameState -> Renderer
display gs = let rect = (rectangleRenderer 600 600 red)
                                { pos = Right ( HCenterAlign 0
                                              , VCenterAlign 0
                                              )
                                , children = [ boardRender ]
                                }
                 boardRender = (chessBoard . board $ game gs)
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
                            maybe (select tile) return $ mvSrc gs >>= (`moveTo` tile)

    where clickCoords = if testKeys is [ LeftButton ]
                        then let (x, y) = mousePos is
                             in if x >= 144 && x < 800 - 144
                                && y >=  44 && y < 600 -  44
                                 then Just $ shift ('A', 1) ((x - 144) `div` 64, (y - 44) `div` 64)
                                 else Nothing
                        else Nothing

          select tile = (board $ game gs)!tile >>= \(color, _) -> do guard $ turn (game gs) == color
                                                                     return gs { mvSrc = Just tile }

          moveTo src tile = do gameBoard <- move (board $ game gs) src tile
                               return $ gs { mvSrc = Nothing
                                           , game = (game gs) { board = gameBoard
                                                              , turn = C.next . turn $ game gs
                                                              } 
                                           }

-- | We don't do anything... for now.
update :: GameState -> Double -> InputState -> IO (GameState, [ResourceRequest], Renderer)
update gs !t is = let gs'  = fromMaybe gs (considerMovement gs is)
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
initState = GameState 0 (400, 300) 0 initGame Nothing

-- Call initialization routines. Register callback function to display
-- graphics. Enter main loop and process events.
main :: IO ()
main = configLogger
     >> runGame Config.windowTitle initState update
