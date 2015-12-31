module Data.Name.Internal (
  digits,
) where

digits :: Integral a => a -> a -> [a]
digits base i = scanr (\ each prev -> (prev `div` base) `mod` base) i [0..(logBase (fromIntegral base) (fromIntegral i))]
