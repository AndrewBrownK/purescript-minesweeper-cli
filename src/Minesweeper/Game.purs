module Minesweeper.Game where

import Prelude

import Data.Array (foldr, index)
import Data.Int (fromString)
import Data.List (catMaybes)
import Data.Map (Map, insert, lookup)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set (Set)
import Data.Set as Set
import Data.String (Pattern(..), split, toLower)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Class.Console (clear, log)
import Effect.Random (randomInt)
import Minesweeper.Model (CellState(..), Coord, GameAction(..), GameState, coordNeighbors, gridHeight, gridWidth, qtyBombs, xs, ys)
import Minesweeper.Render (printGameState, printHelp)
import Node.Process (exit)
import Node.ReadLine (Interface)
import Node.ReadLine.Aff (prompt)


--
-- This first section of code pertains to the core loop
--

type LoopState =
  { gameState :: GameState
  , consoleInterface :: Interface
  }

startGame :: Interface -> Aff Unit
startGame consoleInterface = do
  let
    defaultCell = Hidden { bomb: false, flagged: false }
    grid = foldr (\y m -> foldr (\x n -> Map.insert { x, y } defaultCell n) m xs) Map.empty ys
    gameState = { firstRevealDone: false, grid, lost: false, hiddenCells: gridHeight * gridWidth }
  printGameState gameState
  loop { gameState, consoleInterface }

loop :: LoopState -> Aff Unit
loop loopState = do
  rawInput <- prompt loopState.consoleInterface
  handleAction loopState $ interpretInput rawInput

interpretInput :: String -> GameAction
interpretInput line = fromMaybe Help do
  let lineElements = split (Pattern " ") line
  command <- index lineElements 0
  case toLower command of
    "quit" -> Just Quit
    "restart" -> Just Restart
    "reveal" -> do
      twoInts <- readTwoInts lineElements
      Just $ Reveal twoInts
    "flag" -> do
      twoInts <- readTwoInts lineElements
      Just $ Flag twoInts
    _ -> Nothing
  where
    readTwoInts :: Array String -> Maybe { x :: Int, y :: Int }
    readTwoInts args = do
      x' <- index args 1
      y' <- index args 2
      x <- fromString x'
      y <- fromString y'
      pure { x, y }

handleAction :: LoopState -> GameAction -> Aff Unit
handleAction _         Quit = log "Thank you for playing! Bye-bye." <> (liftEffect $ exit 0)
handleAction loopState Restart = startGame loopState.consoleInterface
handleAction loopState Help = printHelp <> loop loopState
handleAction loopState (Flag coord) = do
  let newState = flagCell coord loopState.gameState
  liftEffect clear
  printGameState newState
  loop $ loopState { gameState = newState }
handleAction loopState (Reveal coord) = case loopState.gameState of
  { lost: true } -> loop loopState
  { firstRevealDone: false } -> do
    newState <- initializeBombs coord loopState.gameState
    handleAction (loopState { gameState = newState }) (Reveal coord)
  _ -> do
    let newState = revealCell coord loopState.gameState
    liftEffect clear
    printGameState newState
    if newState.lost
      then handleLoss loopState
      else if (newState.hiddenCells <= qtyBombs)
        then handleWin loopState
        else loop $ loopState { gameState = newState }


--
-- This next section pertains to the very first cell that the player reveals.
-- This is the time at which bombs are distributed across the field.
-- The bombs are not distributed until after the first reveal, so that the
-- first reveal is ALWAYS a successful choice.
--


initializeBombs :: Coord -> GameState -> Aff GameState
initializeBombs { x, y } gameState | x < 0 || x >= gridWidth || y < 0 || y >= gridHeight = pure gameState
initializeBombs coord gameState = do
  bombCoords <- createBombCoords coord Set.empty
  let grid = foldr setBomb gameState.grid bombCoords
  pure { firstRevealDone: true, grid, lost: false, hiddenCells: gameState.hiddenCells }


setBomb :: Coord -> Map Coord CellState -> Map Coord CellState
setBomb coord map = fromMaybe map do
  existing <- lookup coord map
  case existing of
    Hidden { flagged } -> Just $ insert coord (Hidden { flagged, bomb: true }) map
    _ -> Nothing


createBombCoords :: Coord -> Set Coord -> Aff (Set Coord)
createBombCoords forbidden existing = let
  filtered = Set.delete forbidden existing
  in case (Set.size filtered < qtyBombs) of
    false -> pure filtered
    true -> do
      x <- liftEffect $ randomInt 0 (gridWidth-1)
      y <- liftEffect $ randomInt 0 (gridHeight-1)
      createBombCoords forbidden $ Set.insert { x, y } filtered


--
-- This next section pertains to the "Reveal" action in particular
--

countBomb :: CellState -> Int
countBomb (Hidden { bomb: true }) = 1
countBomb Exploded = 1
countBomb _ = 0

revealCell :: Coord -> GameState -> GameState
revealCell { x, y } gameState | x < 0 || x >= gridWidth || y < 0 || y >= gridHeight = gameState
revealCell coord gameState = fromMaybe gameState do
  cellState <- lookup coord gameState.grid
  pure case cellState of
    Exploded -> gameState
    (Revealed _) -> gameState
    (Hidden { flagged: true }) -> gameState
    (Hidden { bomb: true }) -> revealBombs gameState
    (Hidden _) -> let
      adjacentCoords = coordNeighbors coord
      adjacentCellStates = catMaybes $ (\c -> lookup c gameState.grid) <$> adjacentCoords
      bombNeighbors = foldr (\adjacentState count -> count + countBomb adjacentState) 0 adjacentCellStates
      grid = Map.insert coord (Revealed { bombNeighbors }) gameState.grid
      newGameState = { firstRevealDone: true, grid, lost: false, hiddenCells: gameState.hiddenCells - 1 }
      newNewGameState = if (newGameState.hiddenCells <= qtyBombs)
        then revealFlags newGameState
        else newGameState
      in case bombNeighbors of
          0 -> foldr revealCell newNewGameState adjacentCoords
          _ -> newNewGameState

--
-- This next section pertains to the "Flag" action in particular
--



flagCell :: Coord -> GameState -> GameState
flagCell coord gameState = fromMaybe gameState do
  cellState <- lookup coord gameState.grid
  let
    newCellState = case cellState of
      (Hidden { bomb, flagged: false }) -> Hidden { bomb, flagged: true }
      (Hidden { bomb, flagged: true }) -> Hidden { bomb, flagged: false }
      _ -> cellState
    newGrid = insert coord newCellState gameState.grid
  pure $ gameState { grid = newGrid }



--
-- This bit is used for game win/loss.
--

coerceRestartOrQuit :: LoopState -> Aff Unit
coerceRestartOrQuit loopState = do
  rawInput <- prompt loopState.consoleInterface
  let action = interpretInput rawInput
  case action of
    Quit -> handleAction loopState Quit
    _ -> handleAction loopState Restart

--
-- This next section pertains to game loss.
--

explode :: CellState -> CellState
explode (Hidden { bomb: true }) = Exploded
explode other = other

revealBombs :: GameState -> GameState
revealBombs gameState = gameState { grid = explode <$> gameState.grid, lost = true }

handleLoss :: LoopState -> Aff Unit
handleLoss loopState = do
  log "Sorry, you lose :("
  log "Press enter to restart."
  log ""
  coerceRestartOrQuit loopState

--
-- This next section pertains to game win.
--

flagCheat :: CellState -> CellState
flagCheat (Hidden { bomb: true }) = Hidden { bomb: true, flagged: true }
flagCheat other = other

revealFlags :: GameState -> GameState
revealFlags gameState = gameState { grid = flagCheat <$> gameState.grid }

handleWin :: LoopState -> Aff Unit
handleWin loopState = do
  log "You win! Congratulations! B)"
  log "Press enter to restart."
  log ""
  coerceRestartOrQuit loopState















--
