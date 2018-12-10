module Day5 where

-- All thanks to: https://blog.jle.im/entry/alchemical-groups.html
-- learned something awesome!

import Data.Algebra.Free
import Data.Char
import Data.Group
import Data.Group.Free
import Prelude (String)
import Protolude hiding (toList)

inject :: Char -> FreeGroupL Char
inject c
  | isAlpha c && isLower c = returnFree c
  | isAlpha c && isUpper c = invert $ returnFree (toLower c)
  | otherwise              = mempty

part1 :: [Char] -> Int
part1 = length . toList . foldMap inject

clean :: Char -> (FreeGroupL Char -> FreeGroupL Char)
clean c = foldMapFree $ \d ->
  if d == c
  then mempty
  else returnFree d

part2 :: Text -> Int
part2 input = minimum
  [ length . toList $ clean c polymer | c <- ['a' .. 'z'] ]
  where
    polymer = foldMap inject ((toS input) :: String)
