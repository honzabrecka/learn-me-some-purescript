module Tennis where

import Prelude

data Player = Player1 | Player2

data Point = Love | Fifteen | Thirty

data Score
  = Points { player1 :: Point
           , player2 :: Point
           }
  | Fourty { player :: Player
           , secondPlayer :: Point
           }
  | Deuce
  | Advantage Player
  | Game Player


-- Player

instance showPlayer :: Show Player where
  show Player1 = "Player<1>"
  show Player2 = "Player<2>"

derive instance eqPlayer :: Eq Player

-- Point

instance showPoint :: Show Point where
  show Love = "0"
  show Fifteen = "15"
  show Thirty = "30"

derive instance eqPoint :: Eq Point

instance ordPoint :: Ord Point where
  compare Love Love = EQ
  compare Fifteen Fifteen = EQ
  compare Thirty Thirty = EQ
  compare Love _ = LT
  compare Fifteen _ = LT
  compare _ _ = GT

-- Score

instance showScore :: Show Score where
  show (Points { player1, player2 })
    = show Player1 <> " has " <> show player1
    <> " / "
    <> show Player2 <> " has " <> show player2
  show (Fourty { player, secondPlayer })
    = show player <> " has 40"
    <> " / "
    <> (show $ switch player) <> " has " <> show secondPlayer
  show Deuce = "It's deuce."
  show (Advantage player) = show player <> " has advantage."
  show (Game player) = show player <> " won."

-- \\


switch :: Player -> Player
switch Player1 = Player2
switch Player2 = Player1

inc :: Point -> Point
inc Love = Fifteen
inc Fifteen = Thirty
inc _ = Thirty

type BallWinner = Player

update :: Score -> BallWinner -> Score
update (Points { player1: Thirty, player2 }) Player1 = Fourty { player: Player1, secondPlayer: player2 }
update (Points { player1, player2: Thirty }) Player2 = Fourty { player: Player2, secondPlayer: player1 }
update (Points { player1, player2 }) Player1 = Points { player1: inc player1, player2: player2 }
update (Points { player1, player2 }) Player2 = Points { player1: player1, player2: inc player2 }
update (Fourty { player, secondPlayer }) player'
  | player' == player = Game player'
  | player' == switch player && secondPlayer == Thirty = Deuce
  | otherwise = Fourty { player: player, secondPlayer: inc secondPlayer }
update Deuce player = Advantage player
update (Advantage player) player'
  | player' == player = Game player'
  | otherwise = Deuce
update score _ = score





update' :: Score -> Player -> Score
update' (Points { player1: Thirty, player2 }) Player1 = Fourty { player: Player1, secondPlayer: player2 }
update' (Points { player1, player2: Thirty }) Player2 = Fourty { player: Player2, secondPlayer: player1 }
update' (Points { player1, player2 }) Player1 = Points { player1: inc player1, player2: player2 }
update' (Points { player1, player2 }) Player2 = Points { player1: player1, player2: inc player2 }
update' (Fourty { player: Player1, secondPlayer: Thirty }) Player2 = Deuce
update' (Fourty { player: Player2, secondPlayer: Thirty }) Player1 = Deuce
update' (Fourty { player: Player1, secondPlayer }) Player2 = Fourty { player: Player1, secondPlayer: inc secondPlayer }
update' (Fourty { player: Player2, secondPlayer }) Player1 = Fourty { player: Player2, secondPlayer: inc secondPlayer }
update' (Fourty { player: Player1 }) Player1 = Game Player1
update' (Fourty { player: Player2 }) Player2 = Game Player2
update' Deuce Player1 = Advantage Player1
update' Deuce Player2 = Advantage Player2
update' (Advantage Player1) Player1 = Game Player1
update' (Advantage Player2) Player2 = Game Player2
update' (Advantage Player1) Player2 = Deuce
update' (Advantage Player2) Player1 = Deuce
update' score _ = score
