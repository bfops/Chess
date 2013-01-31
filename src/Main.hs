{-# LANGUAGE QuasiQuotes, OverloadedStrings, BangPatterns, CPP #-}
module Main (main) where

import Prelewd hiding ((!))

import IO
import Impure

import Config
import Control.DeepSeq
import Data.Array
import qualified Data.HashMap.Strict as M
import Data.HashString
import Game.Engine
import Game.Input
import qualified Game.Gameplay as G
import Game.Physics.Collision()
import Game.Physics.Integration()
import Game.Render
import Game.Render.Colors
import Game.Resource.Loader
import Game.SceneGraph()
import Storage.List
import System.IO (stderr)
import System.Log.Formatter
import System.Log.Handler as H
import System.Log.Handler.Simple
import System.Log.Logger as L
import Text.Show
import Util.Defs

-- | Initializes all the loggers' states to what was defined in the config file.
configLogger :: SystemIO ()
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
                  traverse_ addLogLevel Config.customLogLevels

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
curGame = (<?> error "No game") . head . game

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
                            (False, True)  -> gs' `withDisabledKeys` \_-> disabledKeys'
            where
                -- | Removes an element from a list, returning whether or not anything was removed.
                remove x = foldl' (\(!b, xs) y -> if x == y then (True, xs) else (b, y:xs)) (False, [])

#define DEFPIECE(name) ([hashed|name|], [texRend|name|])

pieceRenderer :: G.Color -> G.Piece -> Renderer
pieceRenderer c pce = M.lookup (fileString c pce) pieceMap
                    <?> error ("Couldn't find " <> show c <> " " <> show pce <> " to render")
    where
        pieceMap = M.fromList
                [ DEFPIECE("piece-b-b.png")
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
fileString c p = toHashString $ "piece-" <> (colorString c) <> "-" <> (pieceString p) <> ".png"
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

withChildren :: Renderer -> [Maybe Renderer] -> Renderer
withChildren r c = r { children = mapMaybe id c }

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
                                            `withChildren` [renderTileContents (G.shift ('A', 1) p)]
            where checkerRender |     evenx &&     eveny = b
                                | not evenx && not eveny = b
                                | otherwise              = w
                  evenx = even x
                  eveny = even y
                  renderTileContents p' = guard (not $ isMoving p') >> renderPiece <$> gameBoard!p'
                  renderPiece (c, pce, _) = (pieceRenderer c pce)
                                            { pos = Right (CenterAlign 0, CenterAlign 0) }

                  isMoving brdPos = maybe False (== brdPos) $ mvSrc gs

        withPosition :: Renderer -> Coord -> Renderer
        withPosition r c = r { pos = Left c }

        atIndex :: Renderer -> Coord -> Renderer
        atIndex r = withPosition r . idx2pos

        (dx, dy) = rendDims w

display :: GameState -> Coord -> Renderer
display gs (x, y) = (rectangleRenderer 600 600 red)
                        { pos = Right ( CenterAlign 0
                                      , CenterAlign 0
                                      )
                        } `withChildren` [ Just boardRender, moveRender ]
                    where boardRender = (chessBoard gs)
                                           { pos = Right ( CenterAlign 0
                                                         , CenterAlign 0
                                                         )
                                            , rotation = rectRot gs
                                            }
                          moveRender = do mv <- mvSrc gs
                                          (c, pce, _) <- (G.board $ curGame gs)!mv
                                          return $ (pieceRenderer c pce)
                                                   { pos = Left (x - 144, y - 44) }

solveNewRot :: Double -> Double -> InputState -> Double
solveNewRot r dt is = r + v*dt * fromIntegral
                                 ((fromEnum $ testKeys is [ KeyChar 'z' ])
                                - (fromEnum $ testKeys is [ KeyChar 'x' ]))
    where
        v = 1 -- velocity

doMovement :: InputState -> GameState -> GameState
doMovement is gs = (mouseTile >>= mover) <?> gs
    where
          mouseTile = mcond <$> isValidPos <*> tilePos $ mousePos is
          tilePos (x, y) = G.shift ('A', 1) ((x - 144) `div` 64, (y - 44) `div` 64)
          isValidPos (x, y) = (x >= 144) && (x < 800 - 144)
                            && (y >= 44) && (y < 600 - 44)

          select t = gs { mvSrc = Just t }
          place t src = (maybe gs addMove $ G.move (curGame gs) src t) { mvSrc = Nothing }
          addMove m = gs { game = m : (game gs) }

          mover t = if testKeys is [ LeftButton ]
                    then mvSrc gs <&> (\_-> Nothing) <?> Just (select t)
                    else place t <$> mvSrc gs

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
          tryPromote p g = maybe g (\x -> g { game = x:(game g) }) $ G.promote (curGame g) p

doEnd :: GameState -> GameState
doEnd = iff <$> isDone <*> reset <*> id
    where isDone g = G.end (curGame g) <&> (\_-> True) <?> False
          reset gs = gs { game = [G.initGame], mvSrc = Nothing}

update :: GameState -> Double -> InputState -> SystemIO (GameState, [ResourceRequest], Renderer)
update gs !t is = let gs' = foldr ($) gs [doEnd, doMovement is, doPromote is, doUndo is, doRot]
                   in return $!! ( gs'
                                 , [ load [hashed|"chess-square-w.png"|]
                                   , load [hashed|"chess-square-b.png"|]
                                 ] <> map load allPieces
                                 , display gs' (mousePos is)
                                 )
    where
        dt | g0 > 0 = t - g0
           | g0 == 0 = 0 -- only happens on the first update.
           | otherwise = error $ "So apparently, we have a time value less than 0: " <> show g0
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
main :: SystemIO ()
main = configLogger
     >> runGame Config.windowTitle initState update
