{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Helper
import Types
import Graphics.Gloss

width :: Int
width = 1000

height :: Int
height = 1000

offset :: Int
offset = 0

window :: Display
window = InWindow "Comonadic Filters" (width, height) (offset, offset)

background :: Color
background = black

main :: IO ()
main = do
  image <- generateImage
  let randomCoordinates = [(x, y) | y <- [0 .. 9], x <- [0 .. 9]]
      p =
       map (\(x, y) ->
          let x' = x * 50
              y' = y * 50
           in
            case peek (Coord x y) image of
              Nothing -> error "Oh no"
              Just (RGB a b c) ->
                translate
                  (fromIntegral x')
                  (fromIntegral y') $
                  color (makeColorI (fromIntegral a) (fromIntegral b) (fromIntegral c) 100) $
                  rectangleSolid 45 45)
                randomCoordinates
      drawing = pictures p
  print p
  display window background drawing

