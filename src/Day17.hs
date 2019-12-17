{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Day17 where

import           Control.Lens
import           Control.Monad   (guard, when)
import           Data.Char       (chr, ord)
import           Data.Either     (fromRight, isLeft)
import           Data.List       (group, inits, intercalate, isInfixOf)
import           Data.List.Extra (replace)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import           AoC
import           ComputerST
import           Vis

type Point = (Int,Int)

type World = Map Point Char

getInput :: IO Instructions
getInput = readInstructions "input/day17"

getMap :: Instructions -> World
getMap prog =
  let e = outputs <$> execute prog
      ls = fmap (zip [0..]) . lines . fmap chr . fromRight [] $ e
  in Map.fromList . mconcat . zipWith (\ls' y -> map (\(x,c) -> ((x,y),c)) ls') ls $ [0..]

displayMap :: World -> String
displayMap m = drawString m (\p -> Map.findWithDefault ' ' p m)

around :: Point -> [Point]
around (x,y) = [(x,y-1), (x-1,y), (x+1,y), (x,y+1)]

neighbors :: World -> Point -> [Point]
neighbors m c = [(x,y) | (x,y) <- around c, Map.lookup (x,y) m `elem` [Just '#', Just '^']]

intersections :: World -> [Point]
intersections m = [(x,y) | x <- [mnx..mxx], y <- [mny..mxy], is (x,y)]
  where
    ((mnx,mny),(mxx,mxy)) = bounds2d m
    is c = Map.lookup c m == Just '#' && ((length $ neighbors m c) == 4)

corners :: World -> [Point]
corners m = [(x,y) | x <- [mnx..mxx], y <- [mny..mxy], is (x,y)]
  where
    ((mnx,mny),(mxx,mxy)) = bounds2d m
    isPath c = Map.lookup c m == Just '#'
    is c = isPath c && (alone || angle)
      where
        ns = neighbors m c
        alone = length ns == 1
        angle = length ns == 2 && (notSame $ fst <$> ns) && (notSame $ snd <$> ns)
        notSame :: Eq a => [a] -> Bool
        notSame = not . null . tail . group

data Dir = N | E | S | W deriving (Show, Bounded, Enum, Eq)

fwd :: Dir -> (Int,Int) -> (Int,Int)
fwd S (x,y) = (x,y+1)
fwd N (x,y) = (x,y-1)
fwd W (x,y) = (x-1,y)
fwd E (x,y) = (x+1,y)

startingPoint :: World -> Point
startingPoint = fst . head . filter ((== '^') . snd) . Map.toList

data Move = L | R | F Int | A | B | C  deriving (Show, Eq, Ord)

turtle :: World -> [Move]
turtle m = opt $ go N (startingPoint m)
  where
    opt []           = []
    opt (F x:F y:xs) = opt (F (x+y):xs)
    opt (x:xs)       = x : opt xs

    go d p
      | canGoFwd = F 1 : go d (fwd d p)
      | canGoRight = R : go (succ' d) p
      | canGoLeft = L : go (pred' d) p
      | otherwise = []

      where
        canGoFwd = canGo (fwd d p)
        canGoRight = canGo (fwd (succ' d) p)
        canGoLeft = canGo (fwd (pred' d) p)

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
  pure . sum . map (\(x,y) -> x * y) $ ints

part2 :: IO Int
part2 = do
  prog <- getInput
  let m = getMap prog
      turt = turtle m
      (a,b,c,d) = compress turt
      cmdseq = (intercalate "\n" $ [moveStr d, moveStr a, moveStr b, moveStr c, "n"]) <> "\n"
      prog' = prog & ix 0 .~ 2
      out = executeIn (ord <$> cmdseq) prog'

  when (isLeft out) $ fail (show out)
  let outs = fromRight [] (outputs <$> out)

  pure $ last outs
