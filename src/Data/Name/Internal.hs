module Data.Name.Internal (
  digits,
  countDigits,
  showNumeral,
  interleave,
) where

import qualified Data.List as List

digits :: Integral a => a -> a -> [a]
digits base i = fst $ foldr nextDigit ([], i) (List.genericReplicate (countDigits base i) 0)
  where nextDigit _ (list, prev) | (next, remainder) <- prev `divMod` base = (remainder : list, next)

countDigits :: Integral a => a -> a -> a
countDigits base i = 1 + floor (logBase (fromIntegral base) (fromIntegral $ abs i))

showNumeral :: Integral i => String -> i -> String
showNumeral "" _ = ""
showNumeral alphabet i = [ alphabet `List.genericIndex` i ]

interleave :: [a] -> [a] -> [a]
interleave [] bs = bs
interleave as [] = as
interleave (x : xs) y = x : interleave y xs
