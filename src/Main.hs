{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Helper
import Types
import Graphics.Gloss
import Control.Comonad

width :: Int
width = 500

height :: Int
height = 500

offset :: Int
offset = 0

window :: Display
window = InWindow "Comonadic Filters" (width, height) (offset, offset)

background :: Color
background = white

main :: IO ()
main = do
  image <- generateImage
  let randCoord = [(x, y) | y <- [0 .. 99], x <- [0 .. 99]]
      drawing1 = pictures (imageToPicture image randCoord)
      image2 = extend blur image
      drawing2 = pictures (imageToPicture image2 randCoord)
  display window background drawing2
  -- animate window background (\_ -> drawing2)

imageToPicture :: Image -> [(Int, Int)] -> [Picture]
imageToPicture img coords =
  map (\(x, y) ->
      let x' = x * 10
          y' = y * 10
      in
        case peek (Coord x y) img of
          Nothing -> error "Oh no"
          Just (RGB a b c) ->
            translate
              (fromIntegral x')
              (fromIntegral y') $
              color (makeColorI (fromIntegral a) (fromIntegral b) (fromIntegral c) 100) $
              rectangleSolid 10 10)
            coords

