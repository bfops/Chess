module Chess
    where

import Data.Array.IArray
import Data.Maybe
import Data.List
import Control.Monad

data Color = White | Black
    deriving (Eq)

data Piece = Pawn | Rook | Knight | Bishop | Queen | King
    deriving (Enum, Eq, Ord)

type File = Char
type Rank = Integer
type Position = (File, Rank)

class Movable a where
    move :: Board -> Position -> Position -> a -> Maybe Board

type Tile = Maybe (Color, Piece)
type Board = Array Position Tile

initBoard :: Board
initBoard = listArray (('A', 1), ('H', 8)) $
                      (backRank White
                        ++ frontRank White
                        ++ (concat $ replicate 4 otherRank)
                        ++ frontRank Black
                        ++ backRank Black)
    where backRank color = map (Just . (color,)) $
                               [Rook .. King] ++ [Bishop .. Rook]
          frontRank color = replicate 8 $ Just (color, Pawn)
          otherRank = replicate 8 Nothing

movePiece :: Board -> Position -> Position -> Maybe Board
movePiece board src dest = do (color, piece) <- board!src
                              guard . not $ isFriendlyFire color $ board!dest
                              move board src dest piece
    where isFriendlyFire :: Color -> Tile -> Bool
          isFriendlyFire color target = fromMaybe False $ fmap (isSameColor color) target
          isSameColor color = (color ==) . fst

step :: (Ord a, Enum a, Ord b, Enum b) => (a, b) -> (a, b) -> (a, b)
step (x1, y1) (x2, y2) = (step' x1 x2, step' y1 y2)
    where step' :: (Ord c, Enum c) => c -> c -> c
          step' x y | x < y = succ x
                    | x > y = pred x
                    | otherwise = x

hasEmptyPath :: Board -> Position -> Position -> Bool
hasEmptyPath board origin dest = all isNothing $ map (board!) $ path
    where path = unfoldr unfoldStep origin
          unfoldStep pos = if pos == dest then Nothing
                           else Just (step pos dest, step pos dest)

instance Movable Piece where
    move _ _ _ _ = Nothing

