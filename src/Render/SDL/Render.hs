module Render.SDL.Render where

import Control.Lens
import Linear
import FRPEngine.Render.SDL.Primitives
import qualified SDL as S
import FRPEngine.Types
import Types

render :: S.Renderer -> Resources -> (GameState, Bool) -> IO Bool
render renderer res (GameState (CameraState zoomLevel) (PhysicalState (Player player fuel) enemies) alive, exit) =
  do
    S.rendererDrawColor renderer S.$= S.V4 0 0 0 255
    S.clear renderer


    case alive of
      True -> renderSpr (player ^. (collObj . obj))
      False -> pure ()

    sequence_ $ renderSpr . (^. (collObj . obj)) <$> enemies

    renderText' ("Eng: " ++ show (floor fuel)) (S.Rectangle (S.P (V2 100 100)) (V2 150 100)) (V4 50 50 50 255)

    S.present renderer
    pure exit
  where
    -- Static stuff center rot at top left
    renderObj' = renderObj (player ^. (collObj . obj . pos)) (flip getSprite res) (fromIntegral zoomLevel) renderer
    renderSpr = renderObj'
    -- renderTerr = renderObj' False
    renderText' = renderText renderer (res ^. font)
    renderPt pos = renderObj' (Obj pos 0 0 (V2 50 50) SobjectSprite2 True)
