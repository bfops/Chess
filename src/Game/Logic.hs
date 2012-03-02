{-# LANGUAGE TupleSections, FlexibleInstances #-}
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
                  ) where

import Data.Array.IArray
import Data.Maybe
import Data.List
import Control.Monad

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

instance Cycle Color

data Piece = Pawn | Rook | Knight | Bishop | Queen | King
    deriving (Enum, Eq, Ord, Show)

type File = Char
type Rank = Int
type Position = (File, Rank)

type Tile = Maybe (Color, Piece)
type Board = Array Position Tile

-- Initial state of the board.
initBoard :: Board
initBoard = listArray (('A', 1), ('H', 8)) $ concat $ transpose [ backRank White
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
          frontRank color = replicate 8 $ Just (color, Pawn)
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
                         movePiece board src dest piece
    where isFriendlyFire :: Color -> Tile -> Bool
          isFriendlyFire color = maybe False (isSameColor color)
          isSameColor color = (color ==) . fst

step :: (Ord a, Enum a, Ord b, Enum b) => (a, b) -> (a, b) -> (a, b)
step (x1, y1) (x2, y2) = (step' x1 x2, step' y1 y2)
    where step' :: (Ord c, Enum c) => c -> c -> c
          step' x y | x < y = succ x
                    | x > y = pred x
                    | otherwise = x

hasEmptyPath :: Board -> Position -> Position -> Bool
hasEmptyPath board origin dest = all isNothing $ map (board!) path
    where path = unfoldr unfoldStep origin
          unfoldStep pos = if pos == dest then Nothing
                           else Just (step pos dest, step pos dest)

movePiece :: Board -> Position -> Position -> Piece -> Maybe Board
movePiece board src dest _ = Just $ board // [ (src, Nothing)
                                             , (dest, board!src)
                                             ]

