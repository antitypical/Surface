module Data.Name.Internal (
  digits,
) where

digits :: Integral a => a -> a -> [a]
digits base i = scanr (\ each prev -> fst (prev `divMod` base)) i [1..(logBase (fromIntegral base) (fromIntegral i))]
