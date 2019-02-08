module Helper where

import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Word (Word8)
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

mkRange :: [(Int, Int)]
mkRange =
  let x = [0..100]
   in [ (a,b) | b <- reverse x,a <- x]

generateCoordinates :: [Coord]
generateCoordinates = map (\(a,b) -> Coord a b) $ sortBy (comparing $ fst) mkRange

randomWord8 :: IO [Word8]
randomWord8 = do
  g <- newStdGen
  return $ take 10500 $ randomRs (minBound, maxBound) g

generateColours :: IO [RGB]
generateColours = do
  randomNumbers <- randomWord8
  let trio = splitEvery 3 randomNumbers
  return $ concatMap genRgb trio
  where
    genRgb (a:b:c:_) = return $ RGB a b c
    genRgb _   = []

splitEvery :: Int -> [a] -> [[a]]
splitEvery _ [] = []
splitEvery n xs = as : splitEvery n bs
  where (as,bs) = splitAt n xs

generateImage :: IO Image
generateImage = do
  colors <- generateColours
  let d = zip generateCoordinates colors
  return $ Store (\x -> safeRgb $ findRgbVal d x) (Coord 0 0)
    where
      findRgbVal :: [(Coord, b)] -> Coord -> [(Coord, b)]
      findRgbVal d x = filter (\(a,_) -> a == x) d

      safeRgb :: [(Coord, RGB)] -> RGB
      safeRgb []        = RGB 00 00 00
      safeRgb ((_,b):_) = b


