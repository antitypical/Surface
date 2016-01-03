module Data.Name.Internal (
  digits,
  countDigits,
  showNumeral,
  interleave,
) where

import qualified Data.List as List

digits :: Integral a => a -> a -> [a]
digits base i = fst $ foldr nextDigit ([], i) (replicate (fromIntegral $ countDigits base i) ())
  where nextDigit _ (list, prev) | (next, remainder) <- prev `divMod` base = (remainder : list, next)

countDigits :: Integral a => a -> a -> a
countDigits base i = 1 + floor (logBase (fromIntegral base) (fromIntegral $ abs i) :: Double)

showNumeral :: Integral i => String -> i -> String
showNumeral "" _ = ""
showNumeral alphabet i = List.genericIndex alphabet <$> digits (List.genericLength alphabet) i

interleave :: [a] -> [a] -> [a]
interleave [] ys = ys
interleave (x : xs) y = x : interleave y xs
