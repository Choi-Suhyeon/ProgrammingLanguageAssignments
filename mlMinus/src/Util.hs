module Util ((.:)) where

infixr 9 .:

(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
f .: g = (f .) . g
