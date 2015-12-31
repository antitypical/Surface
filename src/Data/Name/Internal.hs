module Data.Name.Internal (
  digits,
  countDigits,
) where

digits :: Integral a => a -> a -> [a]
digits base i = fst $ foldr nextDigit ([], i) (replicate (fromIntegral $ countDigits base i) 0)
  where nextDigit _ (list, prev) | (next, remainder) <- prev `divMod` base = (remainder : list, next)

countDigits :: Integral a => a -> a -> a
countDigits base i = 1 + fromIntegral (length [1..logBase (fromIntegral base) (fromIntegral i)])
