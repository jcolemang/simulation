
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Entity ( Entity
              , ID ( ID )
              , Mass ( Mass )
              , Radius ( Radius )
              , Location ( Location )
              , Velocity ( Velocity )
              , distance, mass, velocity
              , createEntity
              ) where

import Control.Lens ( view
                    , over
                    , makeLenses
                    )
import Physics ( PhysicsEntity ( calculateAcceleration
                               , getVelocity
                               , getPosition
                               , updateVelocity
                               , updatePosition
                               , getDistance
                               , positionDelta
                               )
               )
import Data.Aeson ( ToJSON
                  , toJSON
                  , object
                  , (.=)
                  )

newtype ID       = ID Int
newtype Mass     = Mass Double
newtype Radius   = Radius Double
newtype Location = Location (Double, Double)
newtype Velocity = Velocity (Double, Double)

data Entity = Entity
  { _entityId :: Int
  , _mass     :: Double
  , _radius   :: Double
  , _location :: (Double, Double)
  , _velocity :: (Double, Double)
  }

makeLenses ''Entity

instance Eq Entity where
  e1 == e2 =
    view entityId e1 == view entityId e2


instance PhysicsEntity Entity where
  calculateAcceleration a b =
    let g = 0.5
        (deltaX, deltaY) = positionDelta a b
        f = g * view mass a * view mass b / getDistance a b
        angle = atan2 deltaY deltaX
    in (- f * cos angle / view mass a, - f * sin angle / view mass a)
  getVelocity = view velocity
  getPosition = view location
  updateVelocity = over velocity . const
  updatePosition pos = over location (const pos)


instance ToJSON Entity where
  toJSON e =
    object [ "id" .= view entityId e
           , "mass" .= view mass e
           , "radius" .= view radius e
           , "location" .= view location e
           , "velocity" .= view velocity e
           ]



distance :: Entity -> Entity -> Double
distance e1 e2 =
  let (x1, y1) = view location e1
      (x2, y2) = view location e2
  in sqrt $ (x1 - x2) ^ (2::Int) + (y1 - y2) ^ (2::Int)


createEntity :: ID -> Mass -> Radius -> Location -> Velocity -> Entity
createEntity (ID x) (Mass m) (Radius r) (Location l) (Velocity v) =
  Entity
  { _entityId = x
  , _mass = m
  , _radius = r
  , _location = l
  , _velocity = v
  }
