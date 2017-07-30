
module Logging where

import Data.Aeson ( ToJSON
                  , encode
                  )
import Physics ( states
               , SimulationResult
               )
import Control.Lens ( view
                    )
import Data.ByteString.Lazy.Char8 ( unpack
                                  , unlines
                                  )


generateLog :: ToJSON a => SimulationResult a -> String
generateLog result =
  let entities = view states result
  in unpack $ Data.ByteString.Lazy.Char8.unlines $ map encode entities
