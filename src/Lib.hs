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
