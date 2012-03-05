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

import Control.DeepSeq
import Control.Monad
import Data.Array.IArray
import Data.Cycle
import Data.Function
import Data.Maybe
import Data.List
import Data.Tuple

-- | Color of game pieces.
data Color = White | Black
    deriving (Eq, Show, Enum, Bounded)

instance NFData Color

-- | Game pieces.
-- The Bool parameter indicates whether or not the piece has moved.
data Piece = Pawn Bool
           | Rook Bool
           | Knight
           | Bishop
           | Queen
           | King Bool
    deriving (Eq, Ord, Show)

instance NFData Piece where
    rnf (Pawn x) = x `seq` ()
    rnf (Rook x) = x `seq` ()
    rnf (King x) = x `seq` ()
    rnf    x     = x `seq` ()

-- | Horizontal index of a chessgameBoard.
type File = Char
-- | Vertical index of a chessgameBoard.
type Rank = Int
-- | Describe a chessgameBoard position.
type Position = (File, Rank)
type Delta = (Int, Int)

-- | A square in a chessgameBoard either contains nothing or a colored piece.
type Tile = Maybe (Color, Piece)
-- | Basic gameBoard type for describing game state.
type Board = Array Position Tile

-- | Contains all the information to uniquely describe a chess game.
data UniqueGame = UniqueGame { board      :: Board
                             -- ^ The current state of the board.
                             , enPassant :: Maybe Position
                             -- ^ The position eligable for taking under en passant.
                             , turn       :: Color
                             -- ^ Whose turn is it?
                             }

instance NFData UniqueGame where
    rnf (UniqueGame a b c) = rnf a `seq`
                             rnf b `seq`
                             rnf c `seq`
                             ()

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
    where backRank color = map (Just . (color,)) $
                               [Rook False, Knight, Bishop, Queen, King False, Bishop, Knight, Rook False]
          frontRank color = replicate 8 $ Just (color, Pawn False)
          otherRank = replicate 8 Nothing

-- | Initial state of the game.
initGame :: UniqueGame
initGame = UniqueGame initBoard Nothing White

-- | Attempt to move the piece from `src` to `dest` on `gameBoard`.
move :: UniqueGame
     -- ^ The gameBoard to move on
     -> Position
     -- ^ The position of the piece we're moving
     -> Position
     -- ^ Position to move to
     -> Maybe UniqueGame
     -- ^ Nothing if the move is invalid, Just the new gameBoard otherwise.
move game src dest = do (color, piece) <- gameBoard!src
                        guard . not . isFriendlyFire color $ gameBoard!dest
                        guard $ dest `elem` moveAttempts gameBoard src piece
   
                        return game { board = if piece == King False && src `stepTo` dest /= dest
                                              then castle
                                              else makeMove gameBoard src dest
                                    , turn = next $ turn game
                                    }
    where isFriendlyFire :: Color -> Tile -> Bool
          isFriendlyFire color = maybe False (isSameColor color)
          isSameColor color = (color ==) . fst

          gameBoard = board game

          -- Walk from src towards dest (maybe passing it), and keep going until you hit the rook.
          rookPos = last $ straightPath gameBoard src $ step src dest
          castle = makeMove (makeMove gameBoard src dest) rookPos (dest `stepTo` src)

zipT :: (a -> c -> r1) -> (b -> d -> r2) -> (a, b) -> (c, d) -> (r1, r2)
zipT f g (a, b) (c, d) = (f a c, g b d)

cshift :: (Enum a) => a -> Int -> a
cshift x y = toEnum $ fromEnum x + y

shift :: (Enum a) => (a, Int) -> (Int, Int) -> (a, Int)
shift = zipT cshift (+)

step :: Position -> Position -> Delta
step = zipT step' step'
    where step' x y | x < y = 1
                    | x > y = -1
                    | otherwise = 0

-- Take one step from x towards y.
stepTo :: Position -> Position -> Position
stepTo x = (shift x) . (step x)

isOccupied :: Board -> Position -> Bool
isOccupied gameBoard = isJust . (gameBoard!)

isUnoccupied :: Board -> Position -> Bool
isUnoccupied gameBoard = isNothing . (gameBoard!)

-- Return a straight path from `origin` to `dest`, terminating at the edge of
-- the gameBoard, or when you hit another piece (includes that tile, doesn't include `origin`).
straightPath :: Board -> Position -> Delta -> [Position]
straightPath gameBoard origin d = unfoldr stepFunc . Just $ shift origin d
    where stepFunc p = do pos <- p
                          guard $ isValidPosition pos
                          return (pos, nextPos pos)
          nextPos p = do guard $ isUnoccupied gameBoard p
                         return $ shift p d

hasEmptyPath :: Board -> Position -> Position -> Bool
hasEmptyPath gameBoard src dest = (length $ take l $ straightPath gameBoard src $ step src dest) == l
    where l = on max abs dx dy
          (dx, dy) = delta src dest

ediff :: Enum a => a -> a -> Int
ediff = (-) `on` fromEnum

delta :: Position -> Position -> Delta
delta = flip $ zipT ediff (-)

-- Just moves the piece, no checking.
makeMove :: Board -> Position -> Position -> Board
makeMove gameBoard src dest = gameBoard // [ (src, Nothing)
                                   , (dest, gameBoard!src >>= newStatus)
                                   ]
    where newStatus (color, Pawn False) = Just (color, Pawn True)
          newStatus (color, Rook False) = Just (color, Rook True)
          newStatus (color, King False) = Just (color, King True)
          newStatus x = Just x

isValidPosition :: Position -> Bool
isValidPosition (f, r) | f < 'A' = False
                       | f > 'H' = False
                       | r < 1 = False
                       | r > 8 = False
                       | otherwise = True

pawnStep :: Game.Logic.Color -> Int
pawnStep color = if color == White
                 then 1
                 else -1

moveAttempts :: Board -> Position -> Piece -> [Position]
moveAttempts gameBoard src (Pawn hasMoved) = do (condition, d) <- moves
                                                let dest = src `shift` d
                                                guard $ isValidPosition dest
                                                guard $ condition dest
                                                return dest

    where color = fst . fromJust $ gameBoard!src
          doubleMove = if hasMoved
                       then Nothing
                       else Just $ (isUnoccupied gameBoard, (0, pawnStep color * 2))
          moves = (maybe [] (:[]) doubleMove)
                  ++ [ (isUnoccupied gameBoard, ( 0, pawnStep color))
                     , (isOccupied   gameBoard, ( 1, pawnStep color))
                     , (isOccupied   gameBoard, (-1, pawnStep color))
                     ]

moveAttempts gameBoard src (Rook _) = concat $ map (straightPath gameBoard src) edges
    where edges = [ (0, 1)
                  , (0, -1)
                  , (-1, 0)
                  , (1, 0)
                  ]

moveAttempts _ src Knight = filter isValidPosition $ map (shift src) deltas
    where ls = [(x, y) | x <- [1, -1], y <- [2, -2]]
          deltas = map swap ls ++ ls

moveAttempts gameBoard src Bishop = concat $ map (straightPath gameBoard src) corners
    where corners = [ (x, y) | x <- [1, -1], y <- [1, -1] ]

moveAttempts gameBoard src Queen = moveAttempts gameBoard src (Rook False) ++ moveAttempts gameBoard src Bishop

moveAttempts gameBoard src@(_, r) (King moved) = castles ++ filter isValidPosition moves
    where moves = map (shift src) deltas
          deltas = [ (x, y) | x <- [-1 .. 1], y <- [-1 .. 1] ]

          castles = [ src `castleTo` p | p <- rookPos, canCastleTo p ]

          color = fst . fromJust $ gameBoard!src
          canCastleTo p = not moved
                        && hasEmptyPath gameBoard src (stepTo p src)
                        && maybe False isCastleReceiver (gameBoard!p)
          isCastleReceiver (c, p) = c == color && p == Rook False
          castleTo p1 p2 = stepTo (stepTo p1 p2) p2

          rookPos = [ ('A', r)
                    , ('H', r)
                    ]

