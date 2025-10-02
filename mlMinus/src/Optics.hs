module Optics
    ( module GHC.Generics
    , module Lens.Micro
    , module Data.Function
    , module Data.Functor
    ) where

import Data.Function ((&))
import Data.Functor ((<&>))
import GHC.Generics (Generic)
import Lens.Micro ((%~), (.~), (^.))
