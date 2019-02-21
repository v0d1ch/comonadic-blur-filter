{-# LANGUAGE BangPatterns #-}
module Helper
  ( pos
  , peek
  , peeks
  , seek
  , seeks
  , experiment
  , generateColors
  , generateImage
  , blur
  )
  where

import Control.Comonad
import Data.Maybe (fromMaybe)
import qualified Data.Vector as V
import System.Random
import Types

-- Get the store focus
pos :: Store s a -> s
pos (Store _ a) = a

-- Read out the store at some specific position
peek :: s -> Store s a -> a
peek s (Store f _) = f s

-- Modify the current focus, and read the store using the new focus.
peeks :: (s -> s) -> Store s a -> a
peeks f (Store f' s) = f' (f s)

-- Set the current focus
seek :: s -> Store s a -> Store s a
seek s (Store f _) = Store f s

-- Modify the current focus
seeks :: (s -> s) -> Store s a -> Store s a
seeks f (Store f' s) = Store f' (f s)

-- Run an experiment in the store.
experiment :: Functor f => (s -> f s) -> Store s a -> f a
experiment f (Store s a) = fmap s (f a)

randomInt :: IO [Int]
randomInt = do
  g <- newStdGen
  return $ take 30000 $ randomRs (0, 255) g

generateColors :: IO (V.Vector (V.Vector RGB))
generateColors = do
  randomNumbers <- randomInt
  let allColors = map (\(a:b:c:_) -> genRgb a b c) (splitEvery 3 randomNumbers)
  return $ V.fromList (map V.fromList (splitEvery 100 allColors))
  where
    genRgb a b c = RGB a b c

splitEvery :: Int -> [a] -> [[a]]
splitEvery _ [] = []
splitEvery n xs = as : splitEvery n bs
  where (as,bs) = splitAt n xs

generateImage :: IO Image
generateImage = do
  colors <- generateColors
  return $
    Store (\(Coord x y) -> do
         row <- colors V.!? x
         row V.!? y)
      (Coord 0 0)

(+:) :: RGB -> RGB -> RGB
(+:) (RGB a b c) (RGB d e f) =
  RGB (abs $ (a + d) `div` 2) (abs $ (b + e) `div` 2) (abs (c + f) `div` 2)

infixl 6 +:

blur :: Image -> Maybe RGB
blur image = fromMaybe (extract image) $ do
    let self = fromMaybe (extract image)
    topLeft     <- extractNeighbour (-1) (-1)
    top         <- extractNeighbour   0  (-1)
    topRight    <- extractNeighbour   1  (-1)
    right       <- extractNeighbour   1    0
    bottomRight <- extractNeighbour   1    1
    bottom      <- extractNeighbour   0    1
    bottomLeft  <- extractNeighbour (-1)   1
    left        <- extractNeighbour (-1)   0
    return $ Just $
      -- fromIntegral $ (`div` 16) $
      -- self * 4 +
      -- top * 2 + right * 2 + bottom * 2 + left * 2 +
      topLeft +: topRight +: bottomRight +: bottomLeft
  where
    extractNeighbour :: Int -> Int -> Maybe RGB
    extractNeighbour x y =
      let (Coord x' y') = pos image
      in peek (Coord (x' + x) (y' + y)) image

