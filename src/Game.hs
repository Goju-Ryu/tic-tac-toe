module Game
  ( GameState
  , Player(..)
  , Move(..)
  , makeNewGame
  , makeMove
  , swichPlayer
  )
where

data GameState = State 
  { r1 :: (Maybe Player, Maybe Player, Maybe Player)
  , r2 :: (Maybe Player, Maybe Player, Maybe Player)
  , r3 :: (Maybe Player, Maybe Player, Maybe Player)
  }
instance Show GameState where
  show (State r1 r2 r3) = show r1 ++ "\n" ++ show r2 ++ "\n" ++ show r3


data Player = PlayerO | PlayerX deriving (Eq)
instance Show Player where
  show PlayerO = "O"
  show PlayerX = "X"

data Move = TopL | TopM | TopR | MidL | MidM | MidR | BottomL | BottomM | BottomR deriving (Eq, Read)



makeNewGame :: GameState
makeNewGame = State (Nothing, Nothing, Nothing) (Nothing, Nothing, Nothing) (Nothing, Nothing, Nothing) 

swichPlayer :: Player -> Player
swichPlayer p  
    | p == PlayerO = PlayerX
    | p == PlayerX = PlayerO

makeMove ::  Player -> Move -> GameState -> Either GameState (String, GameState)
makeMove p m s= case (m, s)  of
  (TopL, (State (Nothing, r1m, r1r) r2 r3)) -> Left $ State (Just p, r1m, r1r) r2 r3
  (TopM, (State (r1l, Nothing, r1r) r2 r3)) -> Left $ State (r1l, Just p, r1r) r2 r3
  (TopR, (State (r1l, r1m, Nothing) r2 r3)) -> Left $ State (r1l, r1m, Just p) r2 r3
  (MidL, (State r1 (Nothing, r1m, r1r) r3)) -> Left $ State r1 (Just p, r1m, r1r) r3
  (MidM, (State r1 (r1l, Nothing, r1r) r3)) -> Left $ State r1 (r1l, Just p, r1r) r3
  (MidR, (State r1 (r1l, r1m, Nothing) r3)) -> Left $ State r1 (r1l, r1m, Just p) r3
  (BottomL, (State r1 r2 (Nothing, r1m, r1r))) -> Left $ State r1 r2 (Just p, r1m, r1r)
  (BottomM, (State r1 r2 (r1l, Nothing, r1r))) -> Left $ State r1 r2 (r1l, Just p, r1r)
  (BottomR, (State r1 r2 (r1l, r1m, Nothing))) -> Left $ State r1 r2 (r1l, r1m, Just p)
  (_, _)-> Right ("Cannot take already occupied spot!", s)