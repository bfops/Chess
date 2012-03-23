{-# LANGUAGE QuasiQuotes, OverloadedStrings, BangPatterns, CPP #-}
module Main (main) where

import Config
import Control.DeepSeq
import Control.Monad
import Data.Array
import qualified Data.HashMap.Strict as M
import Data.HashString
import Data.List
import Data.Maybe
import Game.Engine
import Game.Input
import qualified Game.Logic as G
import Game.Physics.Collision()
import Game.Physics.Integration()
import Game.Render
import Game.Render.Colors
import Game.Resource.Loader
import Game.SceneGraph()
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

data GameState = GameState { t0      :: !Double -- ^ the time value of the last frame.
                           , rectPos :: Coord
                           , rectRot :: Double -- ^ rotation of the rectangle, in radians.
                           , game    :: [G.GameState] -- ^ The history of all progressions of the
                                                      -- game state.
                           , mvSrc   :: Maybe G.Position --  ^ if the user's selected a piece to move,
                                                         --    they've selected the one here.
                           -- | When a key is first pressed, it is disabled
                           --   until it is raised again. This is useful for
                           --   keys which are only activated once when held,
                           --   such as text in an in-game console.
                           , disabledKeys :: DisabledKeys
                           }

-- | Return the current state of the game.
-- Will error if, somehow, (game gs) is empty.
curGame :: GameState -> G.GameState
curGame = head . game

instance NFData GameState where
    rnf gs = rnf (rectPos gs) `seq`
             rnf (rectRot gs) `seq`
             rnf (game    gs) `seq`
             rnf (mvSrc   gs) `seq`
             ()

type DisabledKeys = [Key]

withDisabledKeys :: GameState -> (DisabledKeys -> DisabledKeys) -> GameState
withDisabledKeys gs f = gs { disabledKeys = f $ disabledKeys gs }

-- | Runs the list of callback if the given keys have been pressed, making sure
--   to flag keys to prevent repeats.
updateDisabledKeys :: GameState
                   -> [(Key, GameState -> GameState)]
                   -- ^ A list of (Key, updateFunction). The update function is
                   --   called whenever the key has been clicked (not held).
                   -> InputState
                   -> GameState
updateDisabledKeys gs us is = foldl' f gs us
    where
        f :: GameState -> (Key, GameState -> GameState) -> GameState
        f gs' (k, u) = let (disabled, disabledKeys') = remove k $ disabledKeys gs'
                        in case (testKeys is [ k ], disabled) of
                            (False, False) -> gs'
                            (True, True)   -> gs'
                            (True, False)  -> u gs' `withDisabledKeys` (k:)
                            (False, True)  -> gs' `withDisabledKeys` const disabledKeys'
            where
                -- | Removes an element from a list, returning whether or not anything was removed.
                remove x = foldl' (\(!b, xs) y -> if x == y then (True, xs) else (b, y:xs)) (False, [])

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
fileString :: G.Color -> G.Piece -> HashString
fileString c p = toHashString $ "piece-" ++ (colorString c) ++ "-" ++ (pieceString p) ++ ".png"
    where colorString G.White = "w"
          colorString G.Black = "b"

          pieceString G.Pawn   = "p"
          pieceString G.Rook   = "r"
          pieceString G.Knight = "n"
          pieceString G.Bishop = "b"
          pieceString G.Queen  = "q"
          pieceString G.King   = "k"

-- Prevents recomputation of our piece hashstrings.
allPieces :: [HashString]
allPieces = [ fileString c p | c <- [G.White, G.Black] , p <- [minBound .. maxBound ] ]

chessBoard :: GameState -> Renderer
chessBoard gs | rendDims w /= rendDims b = error "White and black square textures are not the same size."
              | otherwise               = let renderBoard = [ tileRender (x,y) | x <- [0..7], y <- [0..7] ]
                                           in defaultRenderer { children = renderBoard, rendDims = (dx*8, dy*8) }
    where
        w, b :: Renderer
        w = [texRend|"chess-square-w.png"|]
        b = [texRend|"chess-square-b.png"|]

        gameBoard = G.board $ curGame gs

        idx2pos :: Coord -> Coord
        idx2pos (x, y) = (dx*x, dy*y)

        tileRender :: Coord -> Renderer
        tileRender p@(x, y) = checkerRender `atIndex` p
                                            `withChildren` (renderTileContents $ G.shift ('A', 1) p)
            where checkerRender |     evenx &&     eveny = b
                                | not evenx && not eveny = b
                                | otherwise              = w
                  evenx = even x
                  eveny = even y
                  renderTileContents brdPos = maybe [] renderPiece $ guard (not $ isMoving brdPos)
                                                                   >> gameBoard!brdPos
                  renderPiece (c, pce, _) = [(fromJust $ M.lookup (fileString c pce) pieceMap)
                                            { pos = Right (HCenterAlign 0, VCenterAlign 0) }]

                  isMoving brdPos = maybe False (== brdPos) $ mvSrc gs

        withPosition :: Renderer -> Coord -> Renderer
        withPosition r c = r { pos = Left c }

        atIndex :: Renderer -> Coord -> Renderer
        atIndex r = withPosition r . idx2pos

        withChildren :: Renderer -> [Renderer] -> Renderer
        withChildren r c = r { children = c }

        (dx, dy) = rendDims w

display :: GameState -> Coord -> Renderer
display gs (x, y) = (rectangleRenderer 600 600 red)
                        { pos = Right ( HCenterAlign 0
                                      , VCenterAlign 0
                                      )
                        , children = [ boardRender ] ++ moveRender
                        }
                    where boardRender = (chessBoard gs)
                                           { pos = Right ( HCenterAlign 0
                                                         , VCenterAlign 0
                                                         )
                                            , rotation = rectRot gs
                                            }
                          moveRender = maybeToList $ do mv <- mvSrc gs
                                                        (c, pce, _) <- (G.board $ curGame gs)!mv
                                                        return $ (fromJust $ M.lookup (fileString c pce) pieceMap)
                                                                     { pos = Left (x - 144, y - 44) }


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

doMovement :: InputState -> GameState -> GameState
doMovement is gs = fromMaybe gs $ mouseTile >>= mover

    where mouseTile = do let (x, y) = mousePos is
                         guard $ x >= 144 && x < 800 - 144
                         guard $ y >=  44 && y < 600 -  44
                         return $ G.shift ('A', 1) ((x - 144) `div` 64, (y - 44) `div` 64)

          select t = do guard . isNothing $ mvSrc gs
                        return $ gs { mvSrc = Just t }

          place t = do src <- mvSrc gs
                       return $ (maybe gs (\s -> gs { game = s:(game gs) }) $ G.move (curGame gs) src t)
                                    { mvSrc = Nothing }

          mover t = if testKeys is [ LeftButton ]
                    then select t
                    else place t

doUndo :: InputState -> GameState -> GameState
doUndo is gs = updateDisabledKeys gs [(KeyChar 'u', runUndo)] is
    where
        runUndo gs' = gs' { game = popIfAble (game gs') }
        popIfAble []  = error "No game states to pop. Someone dun goof'd."
        popIfAble [x] = [x]
        popIfAble (_:xs) = xs

doPromote :: InputState -> GameState -> GameState
doPromote is gs = foldr promoteIfPressed gs [ ('q', G.Queen)
                                            , ('b', G.Bishop)
                                            , ('k', G.Knight)
                                            , ('r', G.Rook)
                                            ]
    where promoteIfPressed (k, p) g = updateDisabledKeys g [(KeyChar k, tryPromote p)] is
          tryPromote p g = maybe g (\x -> g { game = x:(game g) }) $ G.promote (head $ game g) p

doEnd :: GameState -> GameState
doEnd gs' = fromMaybe gs' $ do gs <- listToMaybe $ game gs'
                               guard.not $ G.canMove gs
                               return $ gs' { game = [G.initGame], mvSrc = Nothing}

update :: GameState -> Double -> InputState -> IO (GameState, [ResourceRequest], Renderer)
update gs !t is = let gs' = foldr ($) gs [doEnd, doMovement is, doPromote is, doUndo is, doRot]
                   in return $!! ( gs'
                                 , [ Loaded [hashed|"chess-square-w.png"|]
                                   , Loaded [hashed|"chess-square-b.png"|]
                                 ] ++ map Loaded allPieces
                                 , display gs' (mousePos is)
                                 )
    where
        dt | g0 > 0 = t - g0
           | g0 == 0 = 0 -- only happens on the first update.
           | otherwise = error $ "So apparently, we have a time value less than 0: " ++ show g0
            where
                g0 = t0 gs
        doRot gs' = gs' { t0 = t
                        --, rectPos = solveNewPos (rectPos gs') is
                        , rectRot = solveNewRot (rectRot gs') dt is
                        }

initState :: GameState
initState = GameState 0 (400, 300) 0 [G.initGame] Nothing []

-- Call initialization routines. Register callback function to display
-- graphics. Enter main loop and process events.
main :: IO ()
main = configLogger
     >> runGame Config.windowTitle initState update
