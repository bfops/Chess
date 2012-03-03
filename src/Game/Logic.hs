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
                  , hasEmptyPath
                  ) where

import Control.Arrow
import Control.DeepSeq
import Control.Monad
import Data.Array.IArray
import Data.Function
import Data.Maybe
import Data.List
import Util.Defs

data Color = White | Black
    deriving (Eq, Show, Enum, Bounded)

instance NFData Color
instance Cycle Color

data Piece = Pawn | Rook | Knight | Bishop | Queen | King
    deriving (Enum, Eq, Ord, Show)

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
delta = flip $ tupleApply ((-) `on` fromEnum) (-)

-- Just moves the piece, no checking.
makeMove :: Board -> Position -> Position -> Board
makeMove board src dest = board // [ (src, Nothing)
                                   , (dest, board!src)
                                   ]

tryMove :: Board -> Position -> Position -> Piece -> Maybe Board
tryMove board src dest Pawn = (guard $ takeTest
                                    || moveTest
                                    && ( normalTest
                                      || doubleTest
                                       )
                              )
                            >> (return $ makeMove board src dest)

                        -- tryMove is only called when board!src exists.
    where color = fst $ fromJust $ board!src
          mvDelta = delta src dest

          pawnStep :: (Int, Int)
          pawnStep = if color == White
                     then (0, 1)
                     else (0, -1)

          startRank :: Rank
          startRank = if color == White
                      then 2
                      else 7

          takeTest = (isJust $ board!dest)
                   && (abs $ fst mvDelta) == 1
                   && snd mvDelta == snd pawnStep
          moveTest = hasEmptyPath board src dest
          normalTest = mvDelta == pawnStep
          doubleTest = snd src == startRank
                     && mvDelta == second (*2) pawnStep

tryMove board src dest _ = Just $ makeMove board src dest

