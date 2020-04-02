module Minesweeper.Render where

import Prelude

import Data.Array (foldMap, foldr, foldl)
import Data.Map (lookup)
import Data.Maybe (fromMaybe)
import Data.String (length)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Class.Console (clear, log)
import Minesweeper.Model (CellState(..), Config', GameState)


-- | Print an introduction blurb.
printIntroduction :: Aff Unit
printIntroduction = do
  log "Welcome to Minesweeper!"
  log "There is grid of x y coordinates."
  log "The x-axis is horizontal, the y-axis is vertical."


-- | Print out some help on how to play the game.
printHelp :: Aff Unit
printHelp = do
  log ""
  log "Show a cell by typing \"show x y\" where x and y form coordinates in the grid."
  log "Flag a cell as a mine by typing \"flag x y\" where x and y form coordinates in the grid."
  log "Restart a new game by typing \"restart\""
  log "Quit by typing \"quit\""
  log ""


-- | Render a grid cell.
showCell :: CellState -> String
showCell (Hidden { flagged: true }) = "(F)"
showCell (Hidden { flagged: false }) = "[ ]"
showCell (Revealed { bombNeighbors: 0 }) = "   "
showCell (Revealed { bombNeighbors }) = " " <> show bombNeighbors <> " "
showCell Exploded = " * "


-- | Render a grid row.
showRow :: Config' -> GameState -> Int -> String
showRow config gameState y =
  padYAxis y <> foldl (\str x -> append str $ fromMaybe "[ ]" $ showCell <$> lookup { x, y } gameState.grid) "" config.xs


-- | Render a game state.
printGameState :: Config'-> GameState -> Aff Unit
printGameState config gameState = do
  liftEffect clear
  printIntroduction
  printHelp
  foldr (\y aff -> append aff $ log $ showRow config gameState y) (pure unit) config.ys
  log $ "   " <> foldMap padXAxis config.xs
  log ""


-- | Pad out the formatting/spacing of the the Y axis indices.
padYAxis :: Int -> String
padYAxis i = let
  str = show i <> " "
  len = length str
  in case len of
    2 -> "  " <> str
    3 -> " " <> str
    _ -> str


-- | Pad out the formatting/spacing of the X axis indices.
padXAxis :: Int -> String
padXAxis i = let
  str = show i
  len = length str
  in case len of
    2 -> " " <> str
    1 -> "  " <> str
    _ -> str











--
