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
                  ) where

import Control.Arrow
import Control.DeepSeq
import Control.Monad
import Data.Array.IArray
import Data.Function
import Data.Maybe
import Data.List

data Color = White | Black
    deriving (Eq, Show, Enum, Bounded)

instance NFData Color

data Piece = Pawn | Rook | Knight | Bishop | Queen | King
    deriving (Enum, Eq, Ord, Show)

instance NFData Piece

type File = Char
type Rank = Int
type Position = (File, Rank)
type Delta = (Int, Int)

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
                         guard $ canMove board src dest piece
                         return $ makeMove board src dest
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
    where path = unfoldr unfoldStep $ step origin dest
          unfoldStep pos = if pos == dest then Nothing
                           else Just (pos, step pos dest)

delta :: Position -> Position -> Delta
delta = flip $ tupleApply ((-) `on` fromEnum) (-)

-- Just moves the piece, no checking.
makeMove :: Board -> Position -> Position -> Board
makeMove board src dest = board // [ (src, Nothing)
                                   , (dest, board!src)
                                   ]


isDiagonal :: Delta -> Bool
isDiagonal (0, 0) = False
isDiagonal (x, y) = abs x == abs y

isStraightLine :: Delta -> Bool
isStraightLine l = (fst l == 0) /= (snd l == 0)

canMove :: Board -> Position -> Position -> Piece -> Bool
canMove board src dest Pawn = takeTest
                            || moveTest
                            && ( normalTest
                               || doubleTest
                               )

                        -- canMove is only called when board!src exists.
    where color = fst $ fromJust $ board!src
          -- change in position from our move.
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
          moveTest = isNothing (board!dest)
                   && hasEmptyPath board src dest
          normalTest = mvDelta == pawnStep
          doubleTest = snd src == startRank
                     && mvDelta == second (*2) pawnStep
canMove board src dest Rook = (isStraightLine $ delta src dest)
                            && hasEmptyPath board src dest
canMove _ src dest Knight = isL mvScalar
    where isL (2, 1) = True
          isL (1, 2) = True
          isL _ = False
          mvScalar = (abs *** abs) (delta src dest)
canMove board src dest Bishop = isDiagonal (delta src dest)
                              && hasEmptyPath board src dest
canMove board src dest Queen = (isDiagonal mvDelta || isStraightLine mvDelta)
                             && hasEmptyPath board src dest
    where mvDelta = delta src dest
canMove _ src dest King = (abs x + abs y) == 1
    where (x, y) = delta src dest

