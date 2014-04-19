module Data.Bedrock.Misc where

dropLast :: Int -> [a] -> [a]
dropLast n = reverse . drop n . reverse

takeLast :: Int -> [a] -> [a]
takeLast n = reverse . take n . reverse

