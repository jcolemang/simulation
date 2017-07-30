module Main where

import Entity
import Logging
import Physics


main :: IO ()
main =
  let entities =
        [ createEntity (ID 0) (Mass 1) (Radius 1) (Location (0, 0)) (Velocity (0, 0))
        , createEntity (ID 1) (Mass 1) (Radius 1) (Location (1, 1)) (Velocity (0, 0))
        ]
      numSteps = 10
      outputPath = "output/simulation"
  in writeFile outputPath $ generateLog . generateResult (Steps numSteps) $ entities
