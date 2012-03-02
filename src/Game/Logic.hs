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
                  , next
                  , prev
                  , hasEmptyPath
                  ) where

import Control.Arrow
import Control.DeepSeq
import Control.Monad
import Data.Array.IArray
import Data.Maybe
import Data.List

class (Enum a, Bounded a, Eq a) => Cycle a where
    next :: a -> a
    next a = if a == maxBound
             then minBound
             else succ a
    prev :: a -> a
    prev a = if a == minBound
             then maxBound
             else pred a

data Color = White | Black
    deriving (Eq, Show, Enum, Bounded)

instance NFData Color
instance Cycle Color

data Piece = Pawn Bool | Rook | Knight | Bishop | Queen | King
    deriving (Eq, Ord, Show)

instance Enum Piece where
    succ (Pawn _) = Rook
    succ Rook = Knight
    succ Knight = Bishop
    succ Bishop = Queen
    succ Queen = King
    pred King = Queen
    pred Queen = Bishop
    pred Bishop = Knight
    pred Knight = Rook
    pred Rook = Pawn False
    fromEnum (Pawn _) = 0
    fromEnum Rook = 1
    fromEnum Knight = 2
    fromEnum Bishop = 3
    fromEnum Queen = 4
    fromEnum King = 5
    toEnum 0 = Pawn False
    toEnum 1 = Rook
    toEnum 2 = Knight
    toEnum 3 = Bishop
    toEnum 4 = Queen
    toEnum 5 = King
    toEnum _ = Pawn True

instance NFData Piece

type File = Char
type Rank = Int
type Position = (File, Rank)

type Tile = Maybe (Color, Piece)
type Board = Array Position Tile

-- Initial state of the board.
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
                               [Rook .. King] ++ (reverse [Rook .. Bishop])
          frontRank color = replicate 8 $ Just (color, Pawn False)
          otherRank = replicate 8 Nothing

-- Attempt to move the piece from `src` to `dest` on `board`.
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
                         tryMove board src dest piece
    where isFriendlyFire :: Color -> Tile -> Bool
          isFriendlyFire color = maybe False (isSameColor color)
          isSameColor color = (color ==) . fst

tupleApply :: (a -> c -> r1) -> (b -> d -> r2) -> (a, b) -> (c, d) -> (r1, r2)
tupleApply f g (a, b) (c, d) = (f a c, g b d)

step :: (Ord a, Enum a, Ord b, Enum b) => (a, b) -> (a, b) -> (a, b)
step = tupleApply step' step'
    where step' :: (Ord c, Enum c) => c -> c -> c
          step' x y | x < y = succ x
                    | x > y = pred x
                    | otherwise = x

hasEmptyPath :: Board -> Position -> Position -> Bool
hasEmptyPath board origin dest = all isNothing $ map (board!) path
    where path = unfoldr unfoldStep origin
          unfoldStep pos = if pos == dest then Nothing
                           else Just (step pos dest, step pos dest)

delta :: Position -> Position -> (Int, Int)
delta = flip $ tupleApply (\x y -> (fromEnum x) - (fromEnum y)) (-)

-- Just moves the piece, no checking.
makeMove :: Board -> Position -> Position -> Board
makeMove board src dest = board // [ (src, Nothing)
                                   , (dest, moveState =<< board!src)
                                   ]
    where moveState (color, Pawn False) = Just (color, Pawn True)
          moveState x = Just x

-- Try letting a pawn take from src to dest
pawnTake :: Board -> Position -> Position -> Maybe Board
pawnTake board src dest = (guard $ (first abs $ delta src dest) == (1, 1))
                       >> board!dest
                       >> (return $ makeMove board src dest)

tryMove :: Board -> Position -> Position -> Piece -> Maybe Board
tryMove board src dest (Pawn True) = if (second (flipIfBlack $ board!src) $ delta src dest) == (0, 1)
                                     then Just $ makeMove board src dest
                                     else pawnTake board src dest
    where flipIfBlack (Just (White, _)) = id
          flipIfBlack (Just (Black, _)) = negate

tryMove board src dest _ = Just $ makeMove board src dest

