module Chess
    where

import Data.Array

data Color = White | Black
data Piece = Pawn | Rook | Knight | Bishop | Queen | King
             deriving (Enum, Eq, Ord)

type X = Char
type Y = Integer
type Position = (X, Y)

class Movable a where
    allowedMovement :: Board -> a -> Position -> [(X, Y)]
    move :: Board -> Position -> Position -> Color -> a -> Maybe Board

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
                                    move board p1 p2 color piece

step :: (Enum a, Ord a) => a -> a -> a
step x y | x < y = succ x
         | x > y = pred x
         | x == y = x

stepT :: (Ord a, Enum a, Ord b, Enum b) => (a, b) -> (a, b) -> (a, b)
stepT (x1, y1) (x2, y2) = (step x1 x2, step y1 y2)

checkEmptyPath :: Board -> Position -> Position -> Maybe ()
checkEmptyPath board p1@(f1, r1) p2 | p1 == p2 = Just ()
                                    | otherwise = do checkEmpty (board!r1!f1)
                                                     checkEmptyPath board (stepT p1 p2) p2
    where checkEmpty (Just _) = Nothing
          checkEmpty Nothing = Just ()

