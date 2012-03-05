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
import Control.Arrow
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
type Delta = Int

-- | A square in a chessgameBoard either contains nothing or a colored piece.
type Tile = Maybe (Color, Piece)
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

zipT :: (a -> c -> r1) -> (b -> d -> r2) -> (a, b) -> (c, d) -> (r1, r2)
zipT f g (a, b) (c, d) = (f a c, g b d)

cshift :: (Enum a) => a -> Int -> a
cshift x y = toEnum $ fromEnum x + y

shift :: (Enum a) => (a, Int) -> (Int, Int) -> (a, Int)
shift = zipT cshift (+)

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

isUnoccupied :: Board -> Position -> Bool
isUnoccupied gameBoard = isNothing . (gameBoard!)

ediff :: Enum a => a -> a -> Int
ediff = (-) `on` fromEnum

delta :: Position -> Position -> (Delta, Delta)
delta = flip $ zipT ediff (-)

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
    where backRank color = map (Just . (color,)) $
                               [Rook False, Knight, Bishop, Queen, King False, Bishop, Knight, Rook False]
          frontRank color = replicate 8 $ Just (color, Pawn False)
          otherRank = replicate 8 Nothing

-- | Initial state of the game.
initGame :: UniqueGame
initGame = UniqueGame initBoard Nothing White

-- True iff `color` is in check on `board`.
isCheck :: Color -> Board -> Bool
isCheck color gameBoard = case filter (isMyKing.snd) $ assocs gameBoard of
                            [] -> True
                            (kingPos, _):_ -> any (threatens kingPos) $ indices gameBoard
    where isMyKing (Just (c, p)) = c == color && isKing p
          isMyKing _ = False
          isKing (King _) = True
          isKing _ = False
          threatens pos p = isJust $ do (c, piece) <- gameBoard!p
                                        guard $ color /= c
                                        guard . not $ isKing piece
                                        move' shallowGame p pos
          shallowGame = UniqueGame { board = gameBoard
                                   , enPassant = Nothing
                                   , turn = next color
                                   }
                        
-- Same as `move`, but doesn't check for check.
move' :: UniqueGame -> Position -> Position -> Maybe UniqueGame
move' game src dest = do (color, piece) <- gameBoard!src
                         guard . not . isFriendlyFire color $ gameBoard!dest
                         guard $ dest `elem` moveAttempts game src piece

                         let updateGame | piece == King False && (not $ within 1) = castle
                                        | piece == Pawn False && (not $ within 1) = makePassant
                                        | piece == Pawn True && isUnoccupied gameBoard dest = enactPassant
                                        | otherwise = id
   
                         return $ updateGame game { board = makeMove gameBoard src dest
                                                  , turn = next $ turn game
                                                  , enPassant = Nothing
                                                  }

    where isFriendlyFire :: Color -> Tile -> Bool
          isFriendlyFire color = maybe False (isSameColor color)
          isSameColor color = (color ==) . fst

          within' (-1) = (False, src)
          within' n = case within' (n - 1) of
                        (b, p) -> (b || p == dest, p `stepTo` dest)

          -- is `src` within `n` of `dest`?
          within :: Delta -> Bool
          within = fst . within'

          gameBoard = board game

          -- Walk from src towards dest (maybe passing it), and keep going until you hit the rook.
          rookPos = last $ straightPath gameBoard src $ step src dest
          castle g = g { board = makeMove (board g) rookPos (dest `stepTo` src) }

          makePassant g = g { enPassant = Just $ src `stepTo` dest }
          enactPassant g = g { board = board g // [ ((fst dest, snd src), Nothing) ] }

-- | Attempt to move the piece from `src` to `dest` on `gameBoard`.
move :: UniqueGame
     -- ^ The gameBoard to move on
     -> Position
     -- ^ The position of the piece we're moving
     -> Position
     -- ^ Position to move to
     -> Maybe UniqueGame
     -- ^ Nothing if the move is invalid, Just the new gameBoard otherwise.
move game src dest = do newGame <- move' game src dest
                        guard . not . isCheck (prev $ turn newGame) $ board newGame
                        return newGame

-- Return a straight path from `origin` to `dest`, terminating at the edge of
-- the gameBoard, or when you hit another piece (includes that tile, doesn't include `origin`).
straightPath :: Board -> Position -> (Delta, Delta) -> [Position]
straightPath gameBoard origin d = unfoldr stepFunc . Just $ shift origin d
    where stepFunc p = do pos <- p
                          guard $ isValidPosition pos
                          return (pos, nextPos pos)
          nextPos p = do guard $ isUnoccupied gameBoard p
                         return $ shift p d

-- Just moves the piece, no checking.
makeMove :: Board -> Position -> Position -> Board
makeMove gameBoard src dest = gameBoard // [ (src, Nothing)
                                           , (dest, gameBoard!src >>= Just . second newStatus)
                                           ]
    where newStatus (Pawn False) = Pawn True
          newStatus (Rook False) = Rook True
          newStatus (King False) = King True
          newStatus x = x

moveAttempts :: UniqueGame -> Position -> Piece -> [Position]
moveAttempts game src (Pawn hasMoved) = do (condition, d) <- moves
                                           let dest = src `shift` d
                                           guard $ isValidPosition dest
                                           guard $ condition dest
                                           return dest

    where color = fst . fromJust $ gameBoard!src
          gameBoard = board game

          doubleMove = if hasMoved
                       then Nothing
                       else Just $ (isUnoccupied gameBoard, (0, pawnStep color * 2))

          canEnPassant p = fromMaybe False $ (p ==) <$> enPassant game

          moves = (maybe [] (:[]) doubleMove)
                ++ [ (             isUnoccupied gameBoard,               ( 0, pawnStep color))
                   , (liftA2 (||) (isOccupied   gameBoard) canEnPassant, ( 1, pawnStep color))
                   , (liftA2 (||) (isOccupied   gameBoard) canEnPassant, (-1, pawnStep color))
                   ]

          pawnStep clr = if clr == White
                         then 1
                         else -1

moveAttempts game src (Rook _) = concat $ map (straightPath (board game) src) edges
    where edges = [ (0, 1)
                  , (0, -1)
                  , (-1, 0)
                  , (1, 0)
                  ]

moveAttempts _ src Knight = filter isValidPosition $ map (shift src) deltas
    where ls = [(x, y) | x <- [1, -1], y <- [2, -2]]
          deltas = map swap ls ++ ls

moveAttempts game src Bishop = concat $ map (straightPath (board game) src) corners
    where corners = [ (x, y) | x <- [1, -1], y <- [1, -1] ]

moveAttempts game src Queen = moveAttempts game src (Rook False) ++ moveAttempts game src Bishop

moveAttempts game src@(_, r) (King moved) = castles ++ filter isValidPosition moves
    where moves = map (shift src) deltas
          deltas = [ (x, y) | x <- [-1 .. 1], y <- [-1 .. 1] ]
          gameBoard = board game

          castles = [ src `castleTo` p | p <- rookPos, canCastleTo p ]

          color = fst . fromJust $ gameBoard!src
          canCastleTo p = not moved
                        && not (isCheck color gameBoard)
                        && hasCastlePath (p `stepTo` src)
                        && maybe False isCastleReceiver (gameBoard!p)
          isCastleReceiver (c, p) = c == color && p == Rook False
          castleTo p1 p2 = stepTo (stepTo p1 p2) p2

          rookPos = [ ('A', r)
                    , ('H', r)
                    ]

          hasCastlePath dest = length path == l
                             && (l == 0 || isUnoccupied gameBoard (last path))
                             && all (not . isCheck color . makeMove gameBoard src) path
            where l = on max abs dx dy
                  (dx, dy) = delta src dest
                  path = take l $ straightPath gameBoard src $ step src dest

