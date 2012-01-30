module Chess
    where

import Data.Array
import Control.Monad

data Color = White | Black
    deriving (Eq)

data Piece = Pawn | Rook | Knight | Bishop | Queen | King
    deriving (Enum, Eq, Ord)

type X = Char
type Y = Integer
type Position = (X, Y)

class Movable a where
    allowedMovement :: Board -> a -> Position -> [(X, Y)]
    move :: Board -> Position -> Position -> a -> Maybe Board

type Tile = Maybe (Color, Piece)
type Rank = Array X Tile
type Board = Array Y Rank

initBoard :: Board
initBoard = listArray (1, 8) $ map (listArray ('A', 'H')) $
                                   ([backRank White, frontRank White] ++ (replicate 4 otherRank) ++ [frontRank Black, backRank Black])
    where backRank color = map (Just . (color,)) $ [Rook .. King] ++ [Bishop .. Rook]
          frontRank color = replicate 8 $ Just (color, Pawn)
          otherRank = replicate 8 Nothing

movePiece :: Board -> Position -> Position -> Maybe Board
movePiece board p1@(f1, r1) p2 = do (color, piece) <- board!r1!f1
                                    guard $ noFriendlyFire board p2 color
                                    move board p1 p2 piece
    where noFriendlyFire board (f, r) color = case board!r!f of
                                                Nothing -> True
                                                Just (color2, _) -> color /= color2

step :: (Ord a, Enum a, Ord b, Enum b) => (a, b) -> (a, b) -> (a, b)
step (x1, y1) (x2, y2) = (step' x1 x2, step' y1 y2)
    where step' :: (Ord c, Enum c) => c -> c -> c
          step' x y | x < y = succ x
                    | x > y = pred x
                    | x == y = x


hasEmptyPath :: Board -> Position -> Position -> Bool
hasEmptyPath board p1@(f1, r1) p2 = all isEmpty $ map (\(f, r) -> board!r!f) $ path p1 p2
    where isEmpty Nothing = True
          isEmpty _ = False
          path p1 p2 | p1 == p2 = [p1]
                     | otherwise = p1 : path (step p1 p2) p2

