module Main where

import Brick.AttrMap
import Brick.Main
import Brick.Types
import Brick.Util
import Brick.Widgets.Core
import Brick.Widgets.Border
import qualified Data.Vector as V
import Graphics.Vty.Input.Events
import Graphics.Vty.Attributes
import Helper
import Types

type ResourceName = String

app :: [(AttrName, Attr)] -> App (V.Vector (V.Vector RGB)) e ResourceName
app cMap =
  App
    { appDraw = drawTui
    , appChooseCursor = showFirstCursor
    , appHandleEvent = handleTuiEvent
    , appStartEvent = pure
    , appAttrMap = const $ attrMap mempty cMap
    }

colorMap :: V.Vector (V.Vector RGB) -> [(AttrName, Attr)]
colorMap cmap =
  concat $ V.toList $ V.map (\v -> V.toList $ V.map
          (\(RGB a b c) ->
           let color = rgbColor a b c
            in (attrName (show color), fg color)
          ) v
        ) cmap

buildInitialState :: IO (V.Vector (V.Vector RGB))
buildInitialState = generateColors

drawTui :: (V.Vector (V.Vector RGB)) -> [Widget ResourceName]
drawTui i = [ border $ hBox $ concat $ V.map V.toList $ V.map drawCell i]

drawCell :: V.Vector RGB -> V.Vector (Widget n)
drawCell v =
  V.map (\(RGB a b c) ->
    let color = rgbColor a b c
        cStr  = show color
     in (withAttr (attrName cStr)) (fill '𝜆')) v

handleTuiEvent
  :: (V.Vector (V.Vector RGB))
  -> BrickEvent n e
  -> EventM n (Next (V.Vector (V.Vector RGB)))
handleTuiEvent s e =
  case e of
    VtyEvent vtye ->
      case vtye of
        EvKey (KChar 'q') [] -> halt s
        _ -> continue s
    _ -> continue s

main :: IO ()
main = do
  initialState <- generateColors
  _ <- defaultMain (app $ colorMap initialState) initialState
  putStrLn "end"

