{-# LANGUAGE FlexibleInstances #-}

module Vis where

import           Codec.Picture   (PixelRGB8 (..), generateImage, writePng)
import           Data.List       (intercalate)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

white :: PixelRGB8
white = PixelRGB8 255 255 255

black :: PixelRGB8
black = PixelRGB8 0 0 0

red :: PixelRGB8
red = PixelRGB8 255 0 0

green :: PixelRGB8
green = PixelRGB8 0 255 0

blue :: PixelRGB8
blue = PixelRGB8 0 0 255

someColors :: [PixelRGB8]
someColors = [red, green, blue]

class Bounded2D a where
  -- (min x, min y), (max x, max y)
  bounds2d :: a -> ((Int,Int), (Int,Int))

type PixelFun = (Int,Int) -> PixelRGB8

listBounds :: Integral a => [(a,a)] -> ((Int,Int),(Int,Int))
listBounds l = ((fromIntegral $ minimum xs, fromIntegral $ minimum ys),
                (fromIntegral $ maximum xs, fromIntegral $ maximum ys))
  where
    xs = fst <$> l
    ys = snd <$> l

instance Integral a => Bounded2D (Map (a, a) b) where
  bounds2d = listBounds . Map.keys

draw :: Bounded2D a => FilePath -> a -> PixelFun -> IO ()
draw fn o pf = writePng fn (generateImage fromPF w h)
  where
    ((mnx,mny),(mxx,mxy)) = bounds2d o
    w = mxx - mnx + 1
    h = mxy - mny + 1

    -- img x -> object x
    gx x = mnx + x
    gy y = mny + y

    fromPF x y = pf (gx x, gy y)

type CharFun = (Int, Int) -> Char

drawString :: Bounded2D a => a -> CharFun -> String
drawString a cf = intercalate "\n" (map (\y -> map (\x -> fromPF x y) [0.. w - 1]) [0.. h - 1])
  where
    ((mnx,mny),(mxx,mxy)) = bounds2d a
    w = mxx - mnx + 1
    h = mxy - mny + 1

    -- img x -> object x
    gx x = mnx + x
    gy y = mny + y

    fromPF x y = cf (gx x, gy y)
