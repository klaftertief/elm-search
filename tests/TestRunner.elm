module Main (..) where

import Signal exposing (Signal)
import ElmTest exposing (consoleRunner, tapRunner)
import Console exposing (IO, run)
import Task
import Tests
import TestsF


console : IO ()
console =
  consoleRunner TestsF.all


port runner : Signal (Task.Task x ())
port runner =
  run console
