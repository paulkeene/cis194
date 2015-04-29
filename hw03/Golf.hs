module Golf where

import qualified Data.Foldable as F
import qualified Data.List as L
import qualified Data.Map as M

-- Exercise 1

skips :: [a] -> [[a]]
skips xs = F.foldl f [] is
  where
    is = [1..(length xs)]
    xsWithIndeces = zip xs is
    f acc i = acc ++ [map fst $ filter (\(x, i') -> i' `mod` i == 0)
                      xsWithIndeces]


-- Exercise 2

localMaxima :: Ord a => [a] -> [a]
localMaxima = map (!! 1) . filter isMaxima . (subsections 3)

isMaxima :: Ord a => [a] -> Bool
isMaxima (a:b:c:[]) = b > a && b > c
isMaxima _ = False

subsections :: Int -> [a] -> [[a]]
subsections n xs = F.foldl f [] [0..(length xs - n)]
  where
    f acc i = acc ++ [take n $ drop i xs]


-- Exercise 3

histogram :: [Integer] -> String
histogram xs = (stars xs) ++ "==========\n0123456789\n"

stars :: [Integer] -> String
stars xs = F.foldl f "" [1..rowCount]
  where
    freqs = getFreqs xs
    rowCount = snd $ F.maximumBy (\x y -> (snd x) `compare` (snd y))
               (M.assocs freqs)
    f acc i = F.foldl (g i) "" [0..9] ++ "\n" ++ acc
    g j acc i = case M.lookup i freqs of
      (Just x) -> if x >= j
                  then acc ++ "*"
                  else acc ++ " "
      Nothing -> acc ++ " "

getFreqs :: Ord k => [k] -> M.Map k Int
getFreqs = F.foldl (flip (flip (M.insertWith (+)) 1)) M.empty
