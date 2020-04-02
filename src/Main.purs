module Main where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Minesweeper.Game (startGame)
import Node.ReadLine (createConsoleInterface, noCompletion)



main :: Effect Unit
main = launchAff_ do
  consoleInterface <- liftEffect $ createConsoleInterface noCompletion
  startGame consoleInterface









--
