module Minesweeper.Model where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.List (List, fromFoldable)
import Data.Map (Map)


data Config = Config
  { gridWidth :: Int
  , gridHeight :: Int
  , qtyMines :: Int
  }

type Config' =
  { gridWidth :: Int
  , gridHeight :: Int
  , qtyMines :: Int
  , xs :: List Int
  , ys :: List Int
  }



type Coord = { x :: Int, y :: Int }

coordNeighbors :: Coord -> List Coord
coordNeighbors { x, y } = fromFoldable
  [ { x: x+1, y: y+1 }
  , { x: x+1, y: y }
  , { x: x+1, y: y-1 }
  , { x: x, y: y+1 }
  , { x: x, y: y-1 }
  , { x: x-1, y: y+1 }
  , { x: x-1, y: y }
  , { x: x-1, y: y-1 }
  ]


type GameState =
  { firstRevealDone :: Boolean
  , grid :: Map Coord CellState
  , lost :: Boolean
  , hiddenCells :: Int
  }

data CellState
  = Hidden { bomb :: Boolean, flagged :: Boolean }
  | Revealed { bombNeighbors :: Int }
  | Exploded

derive instance genericCellState :: Generic CellState _
instance showCellState :: Show CellState where show = genericShow

data GameAction
  = Reveal Coord
  | Flag Coord
  | Restart
  | Quit
  | Help

derive instance genericGameAction :: Generic GameAction _
instance showGameAction :: Show GameAction where show = genericShow




















--
