{-# LANGUAGE TupleSections #-}
module Game.Logic ( Color(..)
                  , Piece(..)
                  , File
                  , Rank
                  , Position
                  , Tile
                  , Board
                  , UniqueGame(..)
                  , initGame
                  , move
                  , shift
                  ) where

import Control.Applicative
import Control.DeepSeq
import Control.Monad
import Data.Array.IArray
import Data.Cycle
import Data.Maybe
import Data.List
import Data.Tuple
import Data.Tuple.All

-- | Color of game pieces.
data Color = White | Black
    deriving (Eq, Show, Enum, Bounded)

instance NFData Color

-- | Game pieces.
-- The Bool parameter indicates whether or not the piece has moved.
data Piece = Pawn
           | Rook
           | Knight
           | Bishop
           | Queen
           | King
    deriving (Eq, Enum, Bounded, Ord, Show)

instance NFData Piece

-- | Horizontal index of a chessgameBoard.
type File = Char
-- | Vertical index of a chessgameBoard.
type Rank = Int
-- | Describe a chessgameBoard position.
type Position = (File, Rank)
type Delta = Int

-- | A square in a chessgameBoard either contains nothing or a colored piece, with move history.
type Tile = Maybe (Color, Piece, [Position])
-- | Basic gameBoard type for describing game state.
type Board = Array Position Tile

-- | Contains all the information to uniquely describe a chess game.
data UniqueGame = UniqueGame { board      :: Board
                             -- ^ The current state of the board.
                             , enPassant  :: Maybe Position
                             -- ^ The position eligable for taking under en passant.
                             , turn       :: Color
                             -- ^ Whose turn is it?
                             }

instance NFData UniqueGame where
    rnf (UniqueGame a b c) = rnf a `seq`
                             rnf b `seq`
                             rnf c `seq`
                             ()

type Condition = UniqueGame
               -- ^ The game being played.
               -> Position
               -- ^ Position of the piece taking action.
               -> Position
               -- ^ Position onto which the piece is taking action.
               -> Bool
               -- ^ Whether or not the action may take place.

-- Non-deterministic position.
data NPos = Disp (Delta, Delta)
          -- ^ Displacement relative to the current position.
          | Path (Delta, Delta)
          -- ^ A path in the direction given.

type Handler = UniqueGame
             -- ^ The game being played.
             -> Position
             -- ^ The position of the piece taking action.
             -> Position
             -- ^ The position onto which the piece is taking action.
             -> UniqueGame
             -- ^ The state of the game after the action is taken.

data ActionType = Move | Take deriving Eq

data Action = Action { actionType :: ActionType
                     , npos       :: NPos
                     , cond       :: Maybe Condition
                     , handler    :: Maybe Handler
                     }

zipT :: (a -> c -> r1) -> (b -> d -> r2) -> (a, b) -> (c, d) -> (r1, r2)
zipT f g (a, b) (c, d) = (f a c, g b d)

cshift :: (Enum a) => a -> Int -> a
cshift x y = toEnum $ fromEnum x + y

shift :: (Enum a, Enum b) => (a, b) -> (Int, Int) -> (a, b)
shift = zipT cshift cshift

step :: Position -> Position -> (Delta, Delta)
step = zipT step' step'
    where step' x y | x < y = 1
                    | x > y = -1
                    | otherwise = 0

-- Take one step from x towards y.
stepTo :: Position -> Position -> Position
stepTo x = (shift x) . (step x)

isOccupied :: Board -> Position -> Bool
isOccupied gameBoard = isJust . (gameBoard!)

isEmpty :: Board -> Position -> Bool
isEmpty gameBoard = isNothing . (gameBoard!)

isValidPosition :: Position -> Bool
isValidPosition (f, r) | f < 'A' = False
                       | f > 'H' = False
                       | r < 1 = False
                       | r > 8 = False
                       | otherwise = True

-- | Initial state of the game gameBoard.
initBoard :: Board
initBoard = listArray (('A', 1), ('H', 8)) . concat $ transpose [ backRank White
                                                                , frontRank White
                                                                , otherRank
                                                                , otherRank
                                                                , otherRank
                                                                , otherRank
                                                                , frontRank Black
                                                                , backRank Black
                                                                ]
    where backRank color = map (Just . (color,,[])) $ [ Rook, Knight, Bishop, Queen, King, Bishop, Knight, Rook ]
          frontRank color = replicate 8 $ Just (color, Pawn, [])
          otherRank = replicate 8 Nothing

-- | Initial state of the game.
initGame :: UniqueGame
initGame = UniqueGame initBoard Nothing White

destinations :: UniqueGame -> Position -> Action -> [Position]
destinations game src action = filter isValidTarget . evalPos $ npos action
    where evalPos (Disp d) = do let p = shift src d
                                guard $ isValidPosition p
                                return p
          evalPos (Path d) = straightPath (board game) src d

          actionCond Move = isEmpty $ board game
          actionCond Take = isOccupied $ board game

          isValidTarget p = (actionCond (actionType action) p)
                          && fromMaybe True ((\f -> f game src p) <$> cond action)

-- True iff `color` is in check on `board`.
isCheck :: Color -> UniqueGame -> Bool
isCheck color game = case filter (maybe False isKing . snd) . assocs $ board game of
                            [] -> error $ "No " ++ show color ++ " king!"
                            (kingPos, _):_ -> any (threatens kingPos) . indices $ board game
    where threatens pos p = fromMaybe False $ do (c, piece, _) <- (board game)!p
                                                 guard $ color /= c
                                                 return . any (isThreat pos p) $ actionAttempts piece color
                                        
          isKing (c, piece, _) = color == c && piece == King
          isThreat pos p = liftA2 (&&) ((Take ==).actionType) (elem pos . destinations game p)
                        
-- | Attempt to move the piece from `src` to `dest` on `gameBoard`.
move :: UniqueGame
     -- ^ The gameBoard to move on
     -> Position
     -- ^ The position of the piece we're moving
     -> Position
     -- ^ Position to move to
     -> Maybe UniqueGame
     -- ^ Nothing if the move is invalid, Just the new gameBoard otherwise.
move game src dest = do (color, piece, _) <- gameBoard!src
                        guard $ color == turn game
                        guard . not . isFriendlyFire color $ gameBoard!dest

                        let actions = filter (elem dest . destinations game src) $ actionAttempts piece color
                        guard . not $ null actions
                        let moveHandler = fromMaybe (const.const) . handler $ head actions
                            simpleUpdate = game { board = makeMove gameBoard src dest
                                                , turn = next $ turn game
                                                , enPassant = Nothing
                                                }
                            newGame = moveHandler simpleUpdate src dest

                        guard . not $ isCheck color newGame

                        return newGame

    where isFriendlyFire :: Color -> Tile -> Bool
          isFriendlyFire color = maybe False $ (color ==) . sel1

          gameBoard = board game

-- Return a straight path from `origin` to `dest`, terminating at the edge of
-- the gameBoard, or when you hit another piece (includes that tile, doesn't include `origin`).
straightPath :: Board -> Position -> (Delta, Delta) -> [Position]
straightPath gameBoard origin d = unfoldr stepFunc . Just $ shift origin d
    where stepFunc p = do pos <- p
                          guard $ isValidPosition pos
                          return (pos, nextPos pos)
          nextPos p = do guard $ isEmpty gameBoard p
                         return $ shift p d

-- Just moves the piece, no checking.
makeMove :: Board -> Position -> Position -> Board
makeMove gameBoard src dest = gameBoard // [ (src, Nothing)
                                           , (dest, gameBoard!src >>= Just . (\(x, y, z) -> (x, y, src:z)))
                                           ]

moveAndTake :: [NPos] -> [Action]
moveAndTake loci = [ Action t l Nothing Nothing | t <- [Move, Take], l <- loci ]

symmetry :: [(Delta, Delta)] -> [(Delta, Delta)]
symmetry ds = do d <- ds
                 [d, swap d]

actionAttempts :: Piece -> Color -> [Action]
actionAttempts Pawn color = [ Action Move (Disp ( 0, pawnStep)) Nothing           Nothing
                            , Action Move (Disp ( 0, doubStep)) (Just canDouble)  (Just makePassant)
                            , Action Move (Disp ( 1, pawnStep)) (Just canPassant) (Just enactPassant)
                            , Action Move (Disp (-1, pawnStep)) (Just canPassant) (Just enactPassant)
                            , Action Take (Disp ( 1, pawnStep)) Nothing           Nothing
                            , Action Take (Disp (-1, pawnStep)) Nothing           Nothing
                            ]
    where doubStep = 2 * pawnStep
          canDouble game src dest = null (sel3.fromJust $ (board game)!src) && isEmpty (board game) (src `stepTo` dest)

          pawnStep = if color == White
                     then 1
                     else -1

          makePassant game src dest = game { enPassant = Just (src `stepTo` dest) }
          canPassant game _ dest = maybe False (dest ==) $ enPassant game
          enactPassant game (_, r) (f, _) = game { board = (board game) // [ ((f, r), Nothing) ] }

actionAttempts Rook   _ = moveAndTake . map Path . symmetry $ map (0,) [-1, 1]
actionAttempts Knight _ = moveAndTake . map Disp . symmetry $ [ (x, y) | x <- [-1, 1], y <- [-2, 2] ]
actionAttempts Bishop _ = moveAndTake $ map Path [ (x, y) | x <- [-1, 1], y <- [-1, 1] ]
actionAttempts Queen  c = actionAttempts Rook c ++ actionAttempts Bishop c

actionAttempts King color = castleMoves ++ moveAndTake [ Disp (x, y) | x <- [-1 .. 1], y <- [-1 .. 1] ]
    where castleMoves = [ castleMove d | d <- [ (3, 0), (-4, 0) ] ]
          castleMove d@(x, y) = Action Move (Disp (2 * signum x, y)) (Just $ canCastle d) (Just $ castle d)

          castle d game src dest = game { board = makeMove (board game) (shift src d) (dest `stepTo` src) }

          canCastle d@(x, y) game src dest = isValidPosition (shift src d)
                                           && fromMaybe False (null . sel3 <$> (board game)!src)
                                           && fromMaybe False (null . sel3 <$> (board game)!(shift src d))
                                           && all (isEmpty $ board game) (init $ straightPath (board game) src (signum x, y))
                                           && not (any isCheckThreat [src, src `stepTo` dest, dest])
            where isCheckThreat p = isCheck color $ game { board = makeMove (board game) src p
                                                         , turn = next $ turn game
                                                         }

