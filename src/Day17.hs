{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Day17 where

import           Control.Lens
import           Control.Monad   (guard, when)
import           Data.Char       (chr, ord)
import           Data.Either     (fromRight, isLeft)
import           Data.List       (inits, intercalate, isInfixOf)
import           Data.List.Extra (replace)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import           Advent.AoC
import           Advent.TwoD
import           Advent.Vis
import           ComputerST

type World = Map Point Char

getInput :: IO Instructions
getInput = readInstructions "input/day17"

getMap :: Instructions -> World
getMap prog = parseGrid id . fmap chr . fromRight [] $ outputs <$> execute prog

displayMap :: World -> String
displayMap m = drawString m (\p -> Map.findWithDefault ' ' p m)

neighbors :: World -> Point -> [Point]
neighbors m c = [(x,y) | (x,y) <- around c, Map.lookup (x,y) m `elem` [Just '#', Just '^']]

intersections :: World -> [Point]
intersections m = [(x,y) | x <- [mnx..mxx], y <- [mny..mxy], is (x,y)]
  where
    ((mnx,mny),(mxx,mxy)) = bounds2d m
    is c = Map.lookup c m == Just '#' && (length (neighbors m c) == 4)

startingPoint :: World -> Point
startingPoint = fst . head . filter ((== '^') . snd) . Map.toList

data Move = L | R | F Int | A | B | C  deriving (Show, Eq, Ord)

fwd' :: Dir -> (Int,Int) -> (Int,Int)
fwd' dir = fwd (inv dir)
  where
    inv N = S
    inv S = N
    inv x = x

turtle :: World -> [Move]
turtle m = opt $ go N (startingPoint m)
  where
    opt []           = []
    opt (F x:F y:xs) = opt (F (x+y):xs)
    opt (x:xs)       = x : opt xs

    go d p
      | canGoFwd = F 1 : go d (fwd' d p)
      | canGoRight = R : go (succ' d) p
      | canGoLeft = L : go (pred' d) p
      | otherwise = []

      where
        canGoFwd = canGo (fwd' d p)
        canGoRight = canGo (fwd' (succ' d) p)
        canGoLeft = canGo (fwd' (pred' d) p)

        canGo p' = Map.lookup p' m == Just '#'

toCmd :: Move -> String
toCmd L     = "L"
toCmd R     = "R"
toCmd (F x) = show x
toCmd A     = "A"
toCmd B     = "B"
toCmd C     = "C"

moveStr :: [Move] -> String
moveStr = intercalate "," . fmap toCmd

compress :: [Move] -> ([Move], [Move], [Move], [Move])
compress moves = head $ do
  a <- compressSome moves
  guard $ nn a
  let ms' = replace a [A] moves
  b <- compressSome ms'
  guard $ nn b
  let ms'' = replace b [B] ms'
  c <- compressSome ms''
  guard $ nn c
  let ms''' = replace c [C] ms''
  guard $ nn ms'''
  guard $ (length . moveStr $ ms''') < 20

  pure (a,b,c,ms''')

  where
    nn = not . null

    potential = reverse . filter ((<= 20) . length . moveStr) . noCompd . inits . dropWhile compd
    compd = (`elem` [A,B,C])
    noCompd = filter (not . any compd)
    compressSome ms = filter (\l -> l `isInfixOf` drop (length l) ms) . potential $ ms

expand :: ([Move], [Move], [Move], [Move]) -> [Move]
expand (a,b,c,d) = foldr (\(n,h) -> replace [n] h) d [(A,a), (B,b), (C,c)]

part1 :: IO Int
part1 = do
  prog <- getInput
  let m = getMap prog
      ints = intersections m
  pure . sum . map (uncurry (*)) $ ints

part2 :: IO Int
part2 = do
  prog <- getInput
  let m = getMap prog
      turt = turtle m
      (a,b,c,d) = compress turt
      cmdseq = intercalate "\n" [moveStr d, moveStr a, moveStr b, moveStr c, "n"] <> "\n"
      prog' = prog & ix 0 .~ 2
      out = executeIn (ord <$> cmdseq) prog'

  when (isLeft out) $ fail (show out)
  let outs = fromRight [] (outputs <$> out)

  pure $ last outs
