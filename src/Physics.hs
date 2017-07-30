
{-# LANGUAGE TemplateHaskell #-}

module Physics ( PhysicsEntity ( calculateAcceleration
                               , getVelocity
                               , getPosition
                               , updateVelocity
                               , updatePosition
                               , getDistance
                               , positionDelta
                               )
               , SimulationResult
               , steps, states
               , Steps ( Steps )
               , generateResult
               ) where

import Control.Lens


-- | Helpers

combineTuple :: (a -> b -> c) -> (a, a) -> (b, b) -> (c, c)
combineTuple f (a1, a2) (b1, b2) = (f a1 b1, f a2 b2)


-- | Type Classes

class PhysicsEntity a where
  calculateAcceleration :: a -> a -> (Double, Double)
  getVelocity :: a -> (Double, Double)
  getPosition :: a -> (Double, Double)
  updateVelocity :: (Double, Double) -> a -> a
  updatePosition :: (Double, Double) -> a -> a

  getDistance :: a -> a -> Double
  getDistance a b =
    let (x, y) = positionDelta a b
    in sqrt $ (x ^ (2::Int)) + (y ^ (2::Int))

  positionDelta :: a -> a -> (Double, Double)
  positionDelta a b = combineTuple (-) (getPosition a) (getPosition b)


-- | Types

data SimulationResult a = SimulationResult
  { _steps :: Int
  , _states :: [[a]]
  }

makeLenses ''SimulationResult


-- | Calculations

calculateNextVelocity :: (PhysicsEntity a, Eq a)
                      => [a]
                      -> a
                      -> a
calculateNextVelocity es e =
  let validEntities = filter (/= e) es
      newAccel = foldl
                 (combineTuple (+))
                 (0, 0)
                 $ map (calculateAcceleration e) validEntities
  in updateVelocity (combineTuple (+) newAccel $ getVelocity e) e


calculateNextPosition :: (PhysicsEntity a)
                      => a
                      -> a
calculateNextPosition e =
  let pos = getPosition e
      vel = getVelocity e
      newPos = combineTuple (+) pos vel
  in updatePosition newPos e


-- | Simulation

runStep :: (PhysicsEntity a, Eq a) => [a] -> [a]
runStep es = map (calculateNextPosition . calculateNextVelocity es) es


newtype Steps = Steps Int
generateResult :: (Eq a, PhysicsEntity a)
               => Steps
               -> [a]
               -> SimulationResult a
generateResult (Steps numSteps) startEntities =
  SimulationResult
  { _states = take (numSteps + 1) $ iterate runStep startEntities
  , _steps  = numSteps
  }
