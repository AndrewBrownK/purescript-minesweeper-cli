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


printIntroduction :: Aff Unit
printIntroduction = do
  log "Welcome to Minesweeper!"
  log "There is grid of x y coordinates."
  log "The x-axis is horizontal, the y-axis is vertical."

printHelp :: Aff Unit
printHelp = do
  log ""
  log "Reveal a cell by typing \"reveal x y\" where x and y form coordinates in the grid."
  log "Flag a cell as a bomb by typing \"flag x y\" where x and y form coordinates in the grid."
  log "Restart a new game by typing \"restart\""
  log "Quit by typing \"quit\""
  log ""


showCell :: CellState -> String
showCell (Hidden { flagged: true }) = "(F)"
showCell (Hidden { flagged: false }) = "[ ]"
showCell (Revealed { bombNeighbors: 0 }) = "   "
showCell (Revealed { bombNeighbors }) = " " <> show bombNeighbors <> " "
showCell Exploded = " * "


showRow :: Config' -> GameState -> Int -> String
showRow config gameState y =
  padYAxis y <> foldl (\str x -> append str $ fromMaybe "[ ]" $ showCell <$> lookup { x, y } gameState.grid) "" config.xs

printGameState :: Config'-> GameState -> Aff Unit
printGameState config gameState = do
  liftEffect clear
  printIntroduction
  printHelp
  foldr (\y aff -> append aff $ log $ showRow config gameState y) (pure unit) config.ys
  log $ "   " <> foldMap padXAxis config.xs
  log ""


padYAxis :: Int -> String
padYAxis i = let
  str = show i <> " "
  len = length str
  in case len of
    2 -> "  " <> str
    3 -> " " <> str
    _ -> str

padXAxis :: Int -> String
padXAxis i = let
  str = show i
  len = length str
  in case len of
    2 -> " " <> str
    1 -> "  " <> str
    _ -> str











--
