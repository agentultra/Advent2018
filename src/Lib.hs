module Lib where

import Prelude (read)
import Protolude

readInt :: Text -> Int
readInt = read . dropPlus . toS
  where
    dropPlus str@(c:cs) =
      case c of
        '+' -> cs
        _   -> str

combinations :: [a] -> [(a, a)]
combinations xs = [(x, y) | (x:xs') <- tails xs, y <- xs']
