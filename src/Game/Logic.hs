{-# LANGUAGE TupleSections #-}
module Game.Logic ( Color(..)
                  , Piece(..)
                  , File
                  , Rank
                  , Position
                  , Tile
                  , Board
                  , initBoard
                  , move
                  , shift
                  ) where

import Control.DeepSeq
import Control.Monad
import Data.Array.IArray
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

-- | Horizontal index of a chessboard.
type File = Char
-- | Vertical index of a chessboard.
type Rank = Int
-- | Describe a chessboard position.
type Position = (File, Rank)
type Delta = (Int, Int)

-- | A square in a chessboard either contains nothing or a colored piece.
type Tile = Maybe (Color, Piece)
-- | Basic board type for describing game state.
type Board = Array Position Tile

-- | Initial state of the game board.
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

-- | Attempt to move the piece from `src` to `dest` on `board`.
move :: Board
     -- ^ The board to move on
     -> Position
     -- ^ The position of the piece we're moving
     -> Position
     -- ^ Position to move to
     -> Maybe Board
     -- ^ Nothing if the move is invalid, Just the new board otherwise.
move board src dest = do (color, piece) <- board!src
                         guard . not . isFriendlyFire color $ board!dest
                         guard $ dest `elem` moveAttempts board src piece

                         return $ if piece == King False && src `stepTo` dest /= dest
                                  then castle
                                  else makeMove board src dest
    where isFriendlyFire :: Color -> Tile -> Bool
          isFriendlyFire color = maybe False (isSameColor color)
          isSameColor color = (color ==) . fst

          -- Walk from src towards dest (maybe passing it), and keep going until you hit the rook.
          rookPos = last $ straightPath board src $ step src dest
          castle = makeMove (makeMove board src dest) rookPos (dest `stepTo` src)

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
isOccupied board = isJust . (board!)

isUnoccupied :: Board -> Position -> Bool
isUnoccupied board = isNothing . (board!)

-- Return a straight path from `origin` to `dest`, terminating at the edge of
-- the board, or when you hit another piece (includes that tile, doesn't include `origin`).
straightPath :: Board -> Position -> Delta -> [Position]
straightPath board origin d = unfoldr stepFunc . Just $ shift origin d
    where stepFunc p = do pos <- p
                          guard $ isValidPosition pos
                          return (pos, next pos)
          next p = do guard $ isUnoccupied board p
                      return $ shift p d

hasEmptyPath :: Board -> Position -> Position -> Bool
hasEmptyPath board src dest = (length $ take l $ straightPath board src $ step src dest) == l
    where l = on max abs dx dy
          (dx, dy) = delta src dest

ediff :: Enum a => a -> a -> Int
ediff = (-) `on` fromEnum

delta :: Position -> Position -> Delta
delta = flip $ zipT ediff (-)

-- Just moves the piece, no checking.
makeMove :: Board -> Position -> Position -> Board
makeMove board src dest = board // [ (src, Nothing)
                                   , (dest, board!src >>= newStatus)
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
moveAttempts board src (Pawn hasMoved) = do (condition, d) <- moves
                                            let dest = src `shift` d
                                            guard $ isValidPosition dest
                                            guard $ condition dest
                                            return dest

    where color = fst . fromJust $ board!src
          doubleMove = if hasMoved
                       then Nothing
                       else Just $ (isUnoccupied board, (0, pawnStep color * 2))
          moves = (maybe [] (:[]) doubleMove)
                  ++ [ (isUnoccupied board, ( 0, pawnStep color))
                     , (isOccupied   board, ( 1, pawnStep color))
                     , (isOccupied   board, (-1, pawnStep color))
                     ]

moveAttempts board src (Rook _) = concat $ map (straightPath board src) edges
    where edges = [ (0, 1)
                  , (0, -1)
                  , (-1, 0)
                  , (1, 0)
                  ]

moveAttempts _ src Knight = filter isValidPosition $ map (shift src) deltas
    where ls = [(x, y) | x <- [1, -1], y <- [2, -2]]
          deltas = map swap ls ++ ls

moveAttempts board src Bishop = concat $ map (straightPath board src) corners
    where corners = [ (x, y) | x <- [1, -1], y <- [1, -1] ]

moveAttempts board src Queen = moveAttempts board src (Rook False) ++ moveAttempts board src Bishop

moveAttempts board src@(_, r) (King moved) = castles ++ filter isValidPosition moves
    where moves = map (shift src) deltas
          deltas = [ (x, y) | x <- [-1 .. 1], y <- [-1 .. 1] ]

          castles = [ src `castleTo` p | p <- rookPos, canCastleTo p ]

          color = fst . fromJust $ board!src
          canCastleTo p = not moved && hasEmptyPath board src (stepTo p src) && maybe False isCastleReceiver (board!p)
          isCastleReceiver (c, p) = c == color && p == Rook False
          castleTo p1 p2 = stepTo (stepTo p1 p2) p2

          rookPos = [ ('A', r)
                    , ('H', r)
                    ]

