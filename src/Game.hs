module Game
  ( GameState
  , Player
  , makeNewGame
  )
where

data GameState = State 
  { r1 :: (Maybe Player, Maybe Player, Maybe Player)
  , r2 :: (Maybe Player, Maybe Player, Maybe Player)
  , r3 :: (Maybe Player, Maybe Player, Maybe Player)
  }

instance Show GameState where
  show (State r1 r2 r3) = show r1 ++ "\n" ++ show r2 ++ "\n" ++ show r3


data Player = PlayerO | PlayerX deriving (Show, Eq)

data Move = TopL | TopM | TopR | MidL | MidM | MidR | BottomL | BottomM |BottomR



makeNewGame :: GameState
makeNewGame = State (Nothing, Nothing, Nothing) (Nothing, Nothing, Nothing) (Nothing, Nothing, Nothing) 

makeMove :: GameState -> Player -> Move -> GameState
makeMove _ _ _ = makeNewGame 