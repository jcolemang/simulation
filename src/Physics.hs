
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiWayIf #-}

module Physics ( PhysicsEntity ( calculateAcceleration
                               , getVelocity
                               , getPosition
                               , updateVelocity
                               , updatePosition
                               , getDistance
                               , positionDelta
                               , getId
                               , getRadius
                               , collide
                               )
               , SimulationResult
               , steps, states
               , Steps ( Steps )
               , Timestep ( Timestep )
               , generateResult
               ) where

import Control.Lens
import Control.Monad
import Control.Parallel.Strategies ( parListChunk
                                   , using
                                   , rdeepseq
                                   , NFData
                                   )


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
  getId :: a -> Int
  getRadius :: a -> Double

  getDistance :: a -> a -> Double
  getDistance a b =
    let (x, y) = positionDelta a b
    in sqrt $ (x ^ (2::Int)) + (y ^ (2::Int))

  positionDelta :: a -> a -> (Double, Double)
  positionDelta a b = combineTuple (-) (getPosition a) (getPosition b)

  collide :: [a] -> [a]
  collide = id


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
                      => Double
                      -> a
                      -> a
calculateNextPosition ts e =
  let pos = getPosition e
      (velX, velY) = getVelocity e
      newPos = combineTuple (+) pos (velX * ts, velY * ts)
  in updatePosition newPos e


handleCollisions :: (Eq a, PhysicsEntity a) => a -> [a] -> [a]
handleCollisions x xs =
  let colliding = filter (detectCollision x) xs
  in case colliding of
    [a] -> [a]
    cs -> if all (\i -> getId x >= getId i) cs
          then collide cs
          else [] -- someone else will be dealing with it

detectCollision :: (PhysicsEntity a) => a -> a -> Bool
detectCollision a b =
  let d = getDistance a b
      threshold = getRadius a + getRadius b
  in d < threshold


-- | Simulation

runStep :: (PhysicsEntity a, Eq a, NFData a) => Double -> [a] -> [a]
runStep ts es =
  -- let collided = handleCollisions es
  let collided = join $ map (`handleCollisions` es) es
      entities = map (calculateNextPosition ts . calculateNextVelocity collided) collided
  in entities `using` parListChunk 250 rdeepseq


newtype Steps = Steps Int
newtype Timestep = Timestep Double
generateResult :: (Eq a, PhysicsEntity a, NFData a)
               => Steps
               -> Timestep
               -> [a]
               -> SimulationResult a
generateResult (Steps numSteps) (Timestep ts) startEntities =
  SimulationResult
  { _states = take (numSteps + 1) $ iterate (runStep ts) startEntities
  , _steps  = numSteps
  }
