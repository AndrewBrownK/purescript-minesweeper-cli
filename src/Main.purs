module Main where

import Prelude

import Data.Either (Either(..))
import Data.List (find, foldMap, range)
import Data.Maybe (Maybe(..))
import Data.String.Utils (startsWith)
import Data.Validation.Semigroup (invalid, unV, V(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class.Console (log)
import Minesweeper.Game (startGame)
import Minesweeper.Model (Config(..), Config')
import Node.ReadLine (Completer, createConsoleInterface)
import Options.Applicative (Parser, ParserInfo, execParser, help, helper, info, int, long, metavar, option, progDesc, short, showDefault, value, (<**>))
import Record (merge)



main :: Effect Unit
main = do
  baseConfig <- execParser configInfo
  furtherValidation baseConfig \validConfig -> do
    consoleInterface <- createConsoleInterface completer
    launchAff_ $ startGame validConfig consoleInterface


furtherValidation :: Config -> (Config' -> Effect Unit) -> Effect Unit
furtherValidation (Config config) andThen = unV
    (\errs -> foldMap log errs)
    (\c -> andThen $ merge c { xs: range 0 (c.gridWidth - 1), ys: range 0 (c.gridHeight - 1) })
    $ { gridWidth: _, gridHeight: _, qtyMines: _}
      <$> checkWidth config.gridWidth
      <*> checkHeight config.gridHeight
      <*> checkQtyMines config.gridWidth config.gridHeight config.qtyMines
  where
    checkWidth w =
      if w <= 0 then invalid ["The width must be greater than 0."]
      else if w > 99 then invalid ["The width must be less than 99."]
      else V $ Right w
    checkHeight h =
      if h <= 0 then invalid ["The height must be greater than 0."]
      else if h > 99 then invalid ["The height must be less than 99."]
      else V $ Right h
    checkQtyMines w h m =
      if m < 0 then invalid ["The number of mines cannot be negative."]
      else if m >= (w * h) then invalid ["There are too many mines (" <> show m <> ") for the provided area " <> show w <> " x " <> show h <> "."]
      else V $ Right m




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



completer :: Completer
completer input = let
  match = find (startsWith input) ["show ", "flag ", "quit ", "restart ", "help "]
  in case match of
    Just m -> pure { completions: [m], matched: input }
    Nothing -> pure { completions: [], matched: ""}









--
