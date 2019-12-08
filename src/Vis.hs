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

data DrawSpec = DrawSpec {
  width  :: Int,
  height :: Int,
  transX :: Int -> Int,
  transY :: Int -> Int
  }

mkDrawSpec :: Bounded2D a => a -> DrawSpec
mkDrawSpec a = DrawSpec{..}
  where
    ((mnx,mny),(mxx,mxy)) = bounds2d a
    width = mxx - mnx + 1
    height = mxy - mny + 1

    -- img x -> object x
    transX = (mnx +)
    transY = (mny +)

draw :: Bounded2D a => FilePath -> a -> PixelFun -> IO ()
draw fn o pf = writePng fn (generateImage fromPF width height)
  where
    DrawSpec{..} = mkDrawSpec o
    fromPF x y = pf (transX x, transY y)

type CharFun = (Int, Int) -> Char

drawString :: Bounded2D a => a -> CharFun -> String
drawString a cf = intercalate "\n" (map (\y -> map (\x -> fromPF x y) [0.. width - 1]) [0.. height - 1])
  where
    DrawSpec{..} = mkDrawSpec a
    fromPF x y = cf (transX x, transY y)
