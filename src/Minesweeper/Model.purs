module Minesweeper.Model where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.List (List, fromFoldable)
import Data.Map (Map)


-- | This is the program configuration.
-- | This is the amount of configuration handled by the optparse library.
data Config = Config
  { gridWidth :: Int
  , gridHeight :: Int
  , qtyMines :: Int
  }


-- | This is an elaborated program configuration. We only get this after
-- | performing additional validation on the base configuration provided by
-- | optparse. xs and ys is for convenience and peformance, so that game logic
-- | and rendering code does not need to constantly re-derive the possible values
-- | that each grid coordinate can take.
type Config' =
  { gridWidth :: Int
  , gridHeight :: Int
  , qtyMines :: Int
  , xs :: List Int
  , ys :: List Int
  }


-- | This is a grid coordinate.
type Coord = { x :: Int, y :: Int }

-- | This takes a grid coordinate and returns the adjacent coordinates (including diagonal).
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

-- | This is the fundamental game state. The bombs don't exist until after the
-- | first reveal, which is why we have bombsInitialized. We store the mine field
-- | itself in the grid. lost indicates if the game has been lost. hiddenCells
-- | counts down as cells are revealed, and helps us detect the win condition
-- | where only bombs are left hidden.
type GameState =
  { bombsInitialized :: Boolean
  , grid :: Map Coord CellState
  , lost :: Boolean
  , hiddenCells :: Int
  }

-- | This is the state of a cell. It is pretty self explanatory, but of interesting
-- | note is that hidden cells do not know how many bomb neighbors they have. We
-- | only calculate how many bomb neighbors a cell has upon going from Hidden
-- | to Revealed.
data CellState
  = Hidden { bomb :: Boolean, flagged :: Boolean }
  | Revealed { bombNeighbors :: Int }
  | Exploded

derive instance genericCellState :: Generic CellState _
instance showCellState :: Show CellState where show = genericShow

-- | The types of actions the user can perform to play the game.
data GameAction
  = Reveal Coord
  | Flag Coord
  | Restart
  | Quit
  | Help

derive instance genericGameAction :: Generic GameAction _
instance showGameAction :: Show GameAction where show = genericShow




















--
