module Day4 where

import           Control.Applicative (liftA2)
import           Data.Foldable       (elem)
import           Data.List           (foldl', group)
import           Language.Haskell.TH


numlist :: (Int, Int) ->  ([Int] -> Bool) -> [Int]
numlist r p = pwgen r (liftA2 (&&) increasing (p . fmap length . group . show))
  where pwgen (l,h) p' = [x | x <- [l..h], p' x]
        increasing = snd . foldl' (\(prev,o) x -> (x, o && x >= prev)) ('/', True) . show

quantity :: (Int, Int) -> ([Int] -> Bool) -> Int
quantity r = length . numlist r

part1 :: (Int, Int) -> Int
part1 r = quantity r $ any (> 1)

part2 :: (Int, Int) -> Int
part2 r = quantity r (2 `elem`)

-- Below does it at compile time.

genNums :: String -> (Integer,Integer) -> ([Int] -> Bool) -> Q [Dec]
genNums n (l,h) p = pure [
  SigD (mkName n) (AppT ListT (ConT (mkName "Int"))),
  ValD (VarP (mkName n)) (NormalB (ListE nums)) []]
  where nums = [LitE (IntegerL (toInteger x)) | x <- numlist r' p]
        r' = (fromIntegral l, fromIntegral h)

genCount :: String -> (Integer, Integer) -> ([Int] -> Bool) -> Q [Dec]
genCount n (l,h) p = pure [
  SigD (mkName n) (ConT (mkName "Int")),
  ValD (VarP (mkName n)) (NormalB (LitE (IntegerL ans))) []
  ]
  where ans = toInteger $ quantity (fromIntegral l, fromIntegral h) p
