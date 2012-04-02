{-# LANGUAGE TupleSections #-}
-- | All game logic specific to the game of Chess goes here. This keeps all our
--   chess specific code isolated.
module Game.Gameplay ( Color(..)
                     , Piece(..)
                     , GameState(..)
                     , EndState(..)
                     , File
                     , Rank
                     , Position
                     , Board
                     , tests
                     , end
                     , initGame
                     , move
                     , promote
                     , shift
                     ) where

import Control.Applicative
import Control.DeepSeq
import Control.Monad
import Control.MonadCond
import Data.Array.IArray
import Data.Cycle
import Data.Maybe
import Data.List
import Data.Singleton
import Data.Tuple
import Data.Tuple.All
import Test.Framework

-- | Color of game pieces.
data Color = White | Black
    deriving (Eq, Show, Enum, Bounded)

instance NFData Color

-- | Game pieces.
-- The Bool parameter indicates whether or not the piece has moved.
data Piece = Pawn
           | Rook
           | Knight
           | Bishop
           | Queen
           | King
    deriving (Eq, Enum, Bounded, Ord, Show)

instance NFData Piece

-- | Horizontal index of a chess board.
type File = Char
-- | Vertical index of a chess board.
type Rank = Int
-- | Describe a chessgameBoard position.
type Position = (File, Rank)
type Delta = Int

-- | Information pieces carry with them around the board.
type PieceInfo = (Color, Piece, [Position])
type Tile = Maybe PieceInfo
-- | Basic gameBoard type for describing game state.
type Board = Array Position Tile

-- | Contains all the information to describe a chess game.
data GameState = GameState { board     :: Board
                             -- ^ The current state of the board.
                             , enPassant :: Maybe Position
                             -- ^ The position eligable for taking under en passant.
                             , turn      :: Color
                             -- ^ Whose turn is it?
                             , promotion :: Maybe Position
                             -- ^ Does a piece need to be promoted?
                             , taken     :: [PieceInfo]
                             -- ^ Which pieces have been taken from which sides?
                             }

instance NFData GameState where
    rnf (GameState a b c d e) = rnf a `seq`
                                rnf b `seq`
                                rnf c `seq`
                                rnf d `seq`
                                rnf e `seq`
                                ()

type Condition = GameState
               -- ^ The game being played.
               -> Position
               -- ^ Position of the piece taking action.
               -> Position
               -- ^ Position onto which the piece is taking action.
               -> Bool
               -- ^ Whether or not the action may take place.

-- | Non-deterministic position.
data NPos = Disp (Delta, Delta)
          -- ^ Displacement relative to the current position.
          | Path (Delta, Delta)
          -- ^ A path in the direction given.

type Handler = GameState
             -- ^ The game being played.
             -> Position
             -- ^ The position of the piece taking action.
             -> Position
             -- ^ The position onto which the piece is taking action.
             -> GameState
             -- ^ The state of the game after the action is taken.

data ActionType = Move | Take deriving Eq

data Action = Action { actionType :: ActionType
                     , npos       :: NPos
                     , conds      :: [Condition]
                     , handlers   :: [Handler]
                     }

-- | Different states in which the game may end.
data EndState = Win Color
              -- ^ One side has won
              | Tie
              -- ^ The game has ended with no winner.

tests :: [Test]
tests = []

zipT :: (a -> c -> r1) -> (b -> d -> r2) -> (a, b) -> (c, d) -> (r1, r2)
zipT f g (a, b) (c, d) = (f a c, g b d)

cshift :: (Enum a) => a -> Int -> a
cshift x y = toEnum $ fromEnum x + y

-- | Shift a position by a displacement.
shift :: (Enum a, Enum b)
      => (a, b)
      -- ^ Position to shift
      -> (Delta, Delta)
      -- ^ Amount to shift by
      -> (a, b)
      -- ^ Resultant position
shift = zipT cshift cshift

step :: Position -> Position -> (Delta, Delta)
step = zipT step' step'
    where step' x y | x < y = 1
                    | x > y = -1
                    | otherwise = 0

-- Take one step from x towards y.
stepTo :: Position -> Position -> Position
stepTo x = shift x . step x

isOccupied :: Board -> Position -> Bool
isOccupied gameBoard = isJust . (gameBoard!)

isEmpty :: Board -> Position -> Bool
isEmpty gameBoard = isNothing . (gameBoard!)

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
    where backRank color = map (Just . (color,,[])) [ Rook, Knight, Bishop, Queen, King, Bishop, Knight, Rook ]
          frontRank color = replicate 8 $ Just (color, Pawn, [])
          otherRank = replicate 8 Nothing

-- | Initial state of the game.
initGame :: GameState
initGame = GameState initBoard Nothing White Nothing []

destinations :: GameState -> Position -> Action -> [Position]
destinations game src action = filter isValidTarget . evalPos $ npos action
    where evalPos (Disp d) = filter isValidPosition [shift src d]
          evalPos (Path d) = straightPath (board game) src d

          actionCond Move = isEmpty
          actionCond Take = isOccupied

          isValidTarget p = actionCond (actionType action) (board game) p
                          && all (\f -> f game src p) (conds action)

-- True iff `color` is in check on `board`.
isCheck :: Color -> GameState -> Bool
isCheck color game = single errorMsg (isThreatened.fst) . filter (maybe False isKing . snd) . assocs $ board game
    where
          errorMsg = error $ "No " ++ show color ++ " king!"
          threatens :: Position -> Position -> Bool
          threatens d s = maybe False (canTake d s) $ board game ! s
          canTake d s (c, piece, _) = color /= c && any (isThreat d s) (actionAttempts piece c)
                                        
          isKing (c, King, _) = color == c
          isKing _ = False
          isThreat d s a = actionType a == Take && d `elem` destinations game s a
          isThreatened pos = any (threatens pos) . indices $ board game

end :: GameState -> Maybe EndState
end gs = if canMove
         then mcond Tie isMatDraw
         else Just $ if isCheck (turn gs) gs
                     then Win . prev $ turn gs
                     else Tie
    where
          canMove = any (not.null) . mapMaybe myMoves . assocs $ board gs
          isMatDraw = filter (/= King) material `elem` [ [], [ Knight ], [ Bishop ] ]
          material = mapMaybe (fmap sel2) . elems $ board gs
          myMoves (s, t) = t >>= mcond <$> moves s <*> (turn gs ==).sel1
          moves s (c, p, _) = mapMaybe (move gs s) . concatMap (destinations gs s) $ actionAttempts p c

-- | Attempt to move the piece from `src` to `dest` on `gameBoard`.
move :: GameState
     -- ^ The gameBoard to move on
     -> Position
     -- ^ The position of the piece we're moving
     -> Position
     -- ^ Position to move to
     -> Maybe GameState
     -- ^ Nothing if the move is invalid, Just the new state otherwise.
move game src dest = mfilter preconds (gameBoard!src) >>= move' (game { board = makeMove gameBoard src dest
                                                                      , enPassant = Nothing
                                                                      , turn = next $ turn game
                                                                      })
    where isFriendlyFire :: Color -> Tile -> Bool
          isFriendlyFire color = maybe False $ (color ==) . sel1

          preconds (c, _, _) = isNothing (promotion game)
                             && c == turn game
                             && (not . isFriendlyFire c $ gameBoard!dest)
                                 
          actions (c, p, _) = filter (elem dest . destinations game src) $ actionAttempts p c
          move' game0 x@(c, _, _) = mfilter (not.isCheck c) $ fixTakes <$> game' (actions x) game0
          game' as game0 = foldl' (\g f -> f g src dest) game0 . handlers <$> listToMaybe as

          fixTakes gs = gs { taken = maybeToList (board gs ! dest) ++ (taken gs) }

          gameBoard = board game

promote :: GameState       -- ^ The state of the game
        -> Piece           -- ^ The type of piece to promote to.
        -> Maybe GameState -- ^ Nothing if the promotion is invalid, Just the new state otherwise.
promote g piece = guard canPromote >> promotion g >>= fmap <$> game' <*> (board g !)
    where canPromote = piece `elem` [Knight, Rook, Bishop, Queen]
          game' pos (c, _, h) = g { board = (board g) // [(pos, Just (c, piece, h))]
                                  , promotion = Nothing
                                  }

-- Return a straight path from `origin` to `dest`, terminating at the edge of
-- the gameBoard, or when you hit another piece (includes that tile, doesn't include `origin`).
straightPath :: Board -> Position -> (Delta, Delta) -> [Position]
straightPath gameBoard origin d = unfoldr stepFunc . Just $ shift origin d
    where stepFunc p = (\x -> (x, nextPos x)) <$> mfilter isValidPosition p
          nextPos p = mcond (shift p d) $ isEmpty gameBoard p

-- Just moves the piece, no checking.
makeMove :: Board -> Position -> Position -> Board
makeMove gameBoard src dest = gameBoard // [ (src, Nothing)
                                           , (dest, gameBoard!src >>= Just . (\(x, y, z) -> (x, y, src:z)))
                                           ]

moveAndTake :: [NPos] -> [Action]
moveAndTake loci = [ Action t l [] [] | t <- [Move, Take], l <- loci ]

symmetry :: [(Delta, Delta)] -> [(Delta, Delta)]
symmetry = concatMap $ \d -> [d, swap d]

actionAttempts :: Piece -> Color -> [Action]
actionAttempts Pawn color = map addPromoteCheck
                            [ Action Move (Disp ( 0, pawnStep)) []           []
                            , Action Move (Disp ( 0, doubStep)) [canDouble]  [makePassant]
                            , Action Move (Disp ( 1, pawnStep)) [canPassant] [enactPassant]
                            , Action Move (Disp (-1, pawnStep)) [canPassant] [enactPassant]
                            , Action Take (Disp ( 1, pawnStep)) []           []
                            , Action Take (Disp (-1, pawnStep)) []           []
                            ]
    where doubStep = 2 * pawnStep
          canDouble game src dest = null (sel3.fromJust $ board game ! src) && isEmpty (board game) (src `stepTo` dest)

          (pawnStep, endRank) = if color == White
                                then (1, 8)
                                else (-1, 1)

          fixPromote g _ d@(_, r) = g { promotion = mcond d $ r == endRank}

          addPromoteCheck (Action a b c d) = Action a b c (fixPromote:d)

          makePassant game src dest = game { enPassant = Just (src `stepTo` dest) }
          canPassant game _ dest = maybe False (dest ==) $ enPassant game
          enactPassant game (_, r) (f, _) = game { board = board game // [ ((f, r), Nothing) ] }

actionAttempts Rook   _ = moveAndTake . map Path . symmetry $ map (0,) [-1, 1]
actionAttempts Knight _ = moveAndTake . map Disp . symmetry $ [ (x, y) | x <- [-1, 1], y <- [-2, 2] ]
actionAttempts Bishop _ = moveAndTake $ map Path [ (x, y) | x <- [-1, 1], y <- [-1, 1] ]
actionAttempts Queen  c = actionAttempts Rook c ++ actionAttempts Bishop c

actionAttempts King color = castleMoves ++ moveAndTake [ Disp (x, y) | x <- [-1 .. 1], y <- [-1 .. 1] ]
    where castleMoves = [ castleMove d | d <- [ (3, 0), (-4, 0) ] ]
          castleMove d@(x, y) = Action Move (Disp (2 * signum x, y)) [canCastle d] [castle d]

          castle d game src dest = game { board = makeMove (board game) (shift src d) (dest `stepTo` src) }

          canCastle d@(x, y) game src dest = isValidPosition (shift src d)
                                           && fromMaybe False (null . sel3 <$> board game ! src)
                                           && fromMaybe False (null . sel3 <$> board game ! shift src d)
                                           && all (isEmpty $ board game) (init $ straightPath (board game) src (signum x, y))
                                           && not (any isCheckThreat [src, src `stepTo` dest, dest])
            where isCheckThreat p = isCheck color $ game { board = makeMove (board game) src p
                                                         , turn = next $ turn game
                                                         }
