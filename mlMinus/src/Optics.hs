module Optics 
    ( module GHC.Generics
    , module Lens.Micro
    , module Data.Function
    , module Data.Functor
    ) where

import GHC.Generics         (Generic)
import Data.Function        ((&))
import Data.Functor         ((<&>))
import Lens.Micro           ((^.), (%~), (.~))

