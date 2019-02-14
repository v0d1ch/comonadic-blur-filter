module Main where

import Brick.AttrMap
import Brick.Main
import Brick.Types
import Brick.Widgets.Core
import qualified Data.Vector as V
import Graphics.Vty.Input.Events
import Helper
import Types

type ResourceName = String

app :: App (V.Vector (V.Vector RGB)) e ResourceName
app =
  App
    { appDraw = drawTui
    , appChooseCursor = showFirstCursor
    , appHandleEvent = handleTuiEvent
    , appStartEvent = pure
    , appAttrMap = const $ attrMap mempty []
    }

buildInitialState :: IO (V.Vector (V.Vector RGB))
buildInitialState = generateColors

drawTui :: (V.Vector (V.Vector RGB)) -> [Widget ResourceName]
drawTui image = [ vBox $ V.toList $ V.map drawCell image]

drawCell :: V.Vector RGB -> Widget n
drawCell v = str . show $ V.head v

handleTuiEvent :: (V.Vector (V.Vector RGB)) -> BrickEvent n e -> EventM n (Next (V.Vector (V.Vector RGB)))
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
  endState <- defaultMain app initialState
  print "end"

