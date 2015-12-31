module Data.Name.Internal (
  digits,
) where

digits :: Integral a => a -> a -> [a]
digits base i = fst $ foldr (\ _ (list, prev) -> (snd (prev `divMod` base) : list, prev `div` base)) ([], i) [0..(logBase (fromIntegral base) (fromIntegral i))]
