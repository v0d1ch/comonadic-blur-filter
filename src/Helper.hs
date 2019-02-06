module Helper where

import Data.Word (Word8 (..))
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

generateCoordinates :: [Coord]
generateCoordinates = [Coord x x| x <- [0.. 100]]

randomWord8 :: IO [Word8]
randomWord8 = do
  g <- newStdGen
  return $ randomRs (minBound, maxBound) g

generateColours :: IO [RGB]
generateColours = do
  g <- newStdGen
  randomNumbers <- randomWord8
  let trio = take 300 (splitEvery 3 randomNumbers)
  return $ genRgb trio
  where
    genRgb []  = []
    genRgb (a:as) = do
      color1 <- a
      color2 <- a
      color3 <- a
      RGB color1 color2 color3 : genRgb as

splitEvery :: Int -> [a] -> [[a]]
splitEvery _ [] = []
splitEvery n xs = as : splitEvery n bs
  where (as,bs) = splitAt n xs

-- generateImage :: IO [RGB] -> IO Image
-- generateImage colors =  Store (\x -> coord !! x)

