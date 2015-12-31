module Data.Name.Internal (
  digits,
) where

digits :: Integral a => a -> a -> [a]
digits base i = fst $ foldr (\ _ (list, prev) -> (fst (prev `divMod` base) : list, prev `div` base)) ([], i) [1..(logBase (fromIntegral base) (fromIntegral i))]
