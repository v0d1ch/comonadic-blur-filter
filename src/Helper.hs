module Helper where

import Data.List (sortBy)
import Data.Ord (comparing)
import qualified Data.Vector as V
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
generateCoordinates =
  map (\(a, b) -> Coord a b) $ sortBy (comparing $ fst) mkRange

randomWord8 :: IO [Word8]
randomWord8 = do
  g <- newStdGen
  return $ take 100 $ randomRs (minBound, maxBound) g

generateColours :: IO (V.Vector (V.Vector RGB))
generateColours = do
  randomNumbers <- randomWord8
  let trio = V.fromList $ splitEvery 3 randomNumbers
  return $ V.map genRgb trio
  where
    genRgb (a:b:c:_) = V.singleton (RGB a b c)
    genRgb _   = V.empty

splitEvery :: Int -> [a] -> [[a]]
splitEvery _ [] = []
splitEvery n xs = as : splitEvery n bs
  where (as,bs) = splitAt n xs

generateImage :: IO Image
generateImage = do
  colors <- generateColours
  return $
    Store
      (\(Coord x y) -> do
         row <- colors V.!? x
         row V.!? y)
      (Coord 0 0)

