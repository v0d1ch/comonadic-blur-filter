module Main where

import qualified Data.Vector as V
import Helper
import Types
import Graphics.Gloss

width :: Int
width = 1000

height :: Int
height = 1000

offset :: Int
offset = 10

window :: Display
window = InWindow "Pong" (width, height) (offset, offset)

background :: Color
background = black

main :: IO ()
main = do
  image <- generateImage
  let randomCoordinates = [(x, y) | y <- [0..100], x <- [0 .. 100]]
      p =
       concatMap (\(x, y) ->
          case peek (Coord x y) image of
            Nothing -> []
            Just (RGB a b c) ->
             [translate x y $ color (makeColor a b c 100) $ rectangleSolid 30 30]) randomCoordinates
      drawing = pictures p

  display window background drawing

