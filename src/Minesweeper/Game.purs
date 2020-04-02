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
import Minesweeper.Model (CellState(..), Config', Coord, GameAction(..), GameState, coordNeighbors)
import Minesweeper.Render (printGameState, printHelp)
import Node.Process (exit)
import Node.ReadLine (Interface)
import Node.ReadLine.Aff (prompt)


--
-- This first section of code pertains to the core loop
--


-- | The LoopState is just some stuff we regularly need at almost any point
-- | in the game logic/execution.
type LoopState =
  { gameState :: GameState
  , consoleInterface :: Interface
  , config :: Config'
  }

-- | Start a new game of minesweeper.
-- | The config determines the game parameters.
-- | The interface is the CLI that the user operates through.
-- | The game runs in an Aff and returns nothing. It is entirely effectual.
startGame :: Config' -> Interface -> Aff Unit
startGame config consoleInterface = do
  let
    defaultCell = Hidden { bomb: false, flagged: false }
    grid = foldr (\y m -> foldr (\x n -> Map.insert { x, y } defaultCell n) m config.xs) Map.empty config.ys
    gameState = { firstRevealDone: false, grid, lost: false, hiddenCells: config.gridHeight * config.gridWidth }
  printGameState config gameState
  loop { gameState, consoleInterface, config }

-- | The basic loop of the game. Read an input, interpert the input, and then
-- | handle the input.
loop :: LoopState -> Aff Unit
loop loopState = do
  rawInput <- prompt loopState.consoleInterface
  handleAction loopState $ interpretInput rawInput

-- | This represents the logic for interpretting raw user input into game
-- | commands and actions. In case the raw input is not understood, we always
-- | fall back to the Help command.
interpretInput :: String -> GameAction
interpretInput line = fromMaybe Help do
  let lineElements = split (Pattern " ") line
  command <- index lineElements 0
  case toLower command of
    "quit" -> Just Quit
    "restart" -> Just Restart
    "show" -> do
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

-- | This is the meat and potatoes of the game. Here we actually handle the
-- | actions decided upon by the user. Quit, Restart, and Help are relatively
-- | simple, so their implementations are inlined. Flag and Reveal are more
-- | complex, so we have separate named functions for handling those actions
-- | to improve readability.
handleAction :: LoopState -> GameAction -> Aff Unit
handleAction _         Quit = log "Thank you for playing! Bye-bye." <> (liftEffect $ exit 0)
handleAction loopState Restart = startGame loopState.config loopState.consoleInterface
handleAction loopState (Flag coord) | isCoordInBounds loopState coord = handleFlag loopState coord
handleAction loopState (Reveal coord) | isCoordInBounds loopState coord = handleReveal loopState coord
handleAction loopState _ = printHelp <> loop loopState

-- | Check that a command/action coordinate is actually in-bounds of the grid.
isCoordInBounds :: LoopState -> Coord -> Boolean
isCoordInBounds loopState { x, y } = (x < 0 || x >= loopState.config.gridWidth || y < 0 || y >= loopState.config.gridHeight)

-- | Handle the Flag action. This sets a flag on a cell to prevent accidentally
-- | showing/revealing it and triggering a mine.
handleFlag :: LoopState -> Coord -> Aff Unit
handleFlag loopState coord= do
    let newState = flagCell coord loopState.gameState
    liftEffect clear
    printGameState loopState.config newState

-- | Handle the show/reveal action. This is required to win the game, but also
-- | risks losing the game if you click on a mine. The first time this action is
-- | performed in any game will never result in hitting a mine. Therefore the
-- | mines are only actually placed after the first action of this kind.
-- | If the game hasn't been lost yet, and the first move has already been done,
-- | then we check if the player lost, progressed, or won.
handleReveal :: LoopState -> Coord -> Aff Unit
handleReveal loopState coord = case loopState.gameState of
    { lost: true } -> loop loopState
    { firstRevealDone: false } -> do
      newState <- initializeBombs loopState.config coord loopState.gameState
      handleReveal (loopState { gameState = newState }) coord
    _ -> do
      let newState = revealCell loopState.config coord loopState.gameState
      liftEffect clear
      printGameState loopState.config newState
      if newState.lost
        then handleLoss loopState
        else if (newState.hiddenCells <= loopState.config.qtyMines)
          then handleWin loopState
          else loop $ loopState { gameState = newState }

--
-- This next section pertains to the very first cell that the player reveals.
-- This is the time at which bombs are distributed across the field.
-- The bombs are not distributed until after the first reveal, so that the
-- first reveal is ALWAYS a successful choice.
--


-- | Put mines in the mine field. This distribution is random which is why
-- | we require an effects capable monad.
initializeBombs :: Config' -> Coord -> GameState -> Aff GameState
initializeBombs config coord gameState = do
  bombCoords <- createBombCoords config coord Set.empty
  let grid = foldr setBomb gameState.grid bombCoords
  pure { firstRevealDone: true, grid, lost: false, hiddenCells: gameState.hiddenCells }

-- | Set a Hidden cell to be a Hidden cell with a bomb.
setBomb :: Coord -> Map Coord CellState -> Map Coord CellState
setBomb coord map = fromMaybe map do
  existing <- lookup coord map
  case existing of
    Hidden { flagged } -> Just $ insert coord (Hidden { flagged, bomb: true }) map
    _ -> Nothing

-- | Derive a set of random coordinates to place bombs, excluding one special
-- | forbidden coordinate, which is principally the first chosen revealed cell.
createBombCoords :: Config' -> Coord -> Set Coord -> Aff (Set Coord)
createBombCoords config forbidden existing = let
  filtered = Set.delete forbidden existing
  in case (Set.size filtered < config.qtyMines) of
    false -> pure filtered
    true -> do
      x <- liftEffect $ randomInt 0 (config.gridWidth-1)
      y <- liftEffect $ randomInt 0 (config.gridHeight-1)
      createBombCoords config forbidden $ Set.insert { x, y } filtered


--
-- This next section pertains to the "Reveal" action in particular
--

-- | Count how many bombs is in a cell.
countBomb :: CellState -> Int
countBomb (Hidden { bomb: true }) = 1
countBomb Exploded = 1
countBomb _ = 0

-- | Reveal a cell, and automatically reveal neighboring cells if there are no
-- | adjacent mines. If the revealed cell explodes a mine, then lose the game.
-- | If all non-mine cells have been revealed, then win the game.
revealCell :: Config' -> Coord -> GameState -> GameState
revealCell config coord gameState = fromMaybe gameState do
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
      newNewGameState = if (newGameState.hiddenCells <= config.qtyMines)
        then revealFlags newGameState
        else newGameState
      in case bombNeighbors of
          0 -> foldr (revealCell config) newNewGameState adjacentCoords
          _ -> newNewGameState

--
-- This next section pertains to the "Flag" action in particular
--



-- | Flag a cell, or unflag a cell if it is already flagged.
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

-- | After you win or lose a game, you are prompted to restart. However, you
-- | should be allowed to quit as well. This will accept any user input and
-- | coerce it to either a  Quit or Restart action.
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

-- | If a cell has a bomb, explode it.
explode :: CellState -> CellState
explode (Hidden { bomb: true }) = Exploded
explode other = other


-- | Explode all the bombs in the grid.
revealBombs :: GameState -> GameState
revealBombs gameState = gameState { grid = explode <$> gameState.grid, lost = true }


-- | Tell the user that they lost, and prompt them to restart.
handleLoss :: LoopState -> Aff Unit
handleLoss loopState = do
  log "Sorry, you lose :("
  log "Press enter to restart."
  log ""
  coerceRestartOrQuit loopState

--
-- This next section pertains to game win.
--

-- | If a cell has a bomb, then flag it.
flagCheat :: CellState -> CellState
flagCheat (Hidden { bomb: true }) = Hidden { bomb: true, flagged: true }
flagCheat other = other

-- | Flag all bombs on the grid.
revealFlags :: GameState -> GameState
revealFlags gameState = gameState { grid = flagCheat <$> gameState.grid }

-- | Tell the user that they won, and prompt them to restart.
handleWin :: LoopState -> Aff Unit
handleWin loopState = do
  log "You win! Congratulations! B)"
  log "Press enter to restart."
  log ""
  coerceRestartOrQuit loopState















--
