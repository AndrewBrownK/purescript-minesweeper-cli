module Main where

import Prelude

import Data.List (range)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Minesweeper.Game (startGame)
import Minesweeper.Model (Config(..))
import Node.ReadLine (createConsoleInterface, noCompletion)
import Options.Applicative (Parser, ParserInfo(..), execParser, help, helper, info, int, long, metavar, option, progDesc, short, showDefault, value, (<**>))



main :: Effect Unit
main = do
  (Config { gridWidth, gridHeight, qtyMines }) <- execParser configInfo
  consoleInterface <- createConsoleInterface noCompletion
  let config = { gridWidth, gridHeight, qtyMines, xs: range 0 (gridWidth - 1), ys: range 0 (gridHeight - 1) }
  launchAff_ $ startGame config consoleInterface




configInfo :: ParserInfo Config
configInfo = info (configParser <**> helper) (progDesc "Play minesweeper on the command line.")


configParser :: Parser Config
configParser = ado
  gridWidth <- option int
    (  long "width"
    <> short 'w'
    <> help "Width of the grid (in columns)."
    <> showDefault
    <> value 10
    <> metavar "INT"
    )
  gridHeight <- option int
    (  long "height"
    <> short 'h'
    <> help "Height of the grid (in rows)."
    <> showDefault
    <> value 10
    <> metavar "INT"
    )
  qtyMines <- option int
    (  long "mines"
    <> short 'm'
    <> help "The number of mines to sweep."
    <> showDefault
    <> value 10
    <> metavar "INT"
    )

  in Config { gridWidth, gridHeight, qtyMines }














--
