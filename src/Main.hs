module Main where

import Entity
import Logging
import Physics
import Control.Monad ( foldM
                     )
import Control.Monad.State ( evalState
                           )
import System.Random ( randomRs
                     , newStdGen
                     )


main :: IO ()
main =
  let numSteps = 100
      timestep = 0.25
      outputPath = "output/simulation"
      numEntities = 350
      bounds = 1000
  in do
    putStrLn "Beginning execution."
    g <- newStdGen
    let xs = take numEntities $ randomRs (-bounds, bounds) g
    g' <- newStdGen
    let ys = take numEntities $ randomRs (-bounds, bounds) g'
    g'' <- newStdGen
    let masses = take numEntities $ randomRs (1, 20) g''
    let randomLocations = zip3 xs ys masses
    let entities = map
                   (\(x, y, m) -> createEntity
                              (Mass m)
                              (Radius 1)
                              (Location (x, y))
                              (Velocity (0, 0)))
                   randomLocations
    let entityList = evalState
                     (foldM (\ls sm -> sm >>= \e -> return (e:ls)) [] entities)
                     0
    putStrLn "Created entities, beginning simulation."
    writeFile
      outputPath $
      generateLog (generateResult
                   (Steps numSteps)
                   (Timestep timestep)
                   entityList)
