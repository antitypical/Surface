module Data.Name.Internal (
  digits,
) where

digits :: Integral a => a -> a -> [a]
digits base i = fst $ foldr nextDigit ([], i) [0..(logBase (fromIntegral base) (fromIntegral i))]
  where nextDigit _ (list, prev) | (next, remainder) <- prev `divMod` base = (remainder : list, next)
