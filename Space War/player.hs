module Player(
    playerCreate
    movePlayerRight
    --playerCycle
) where

import Graphics.UI.Fungen
import Screen
import Textures
import Types

startPosition = (fromIntegral(0), fromIntegral((snd windowResolution) `div` 2))

playerCreate :: SpaceWarObject
playerCreate = let sprite = Tex texturePlayerSize texturePlayerIndex
               in object "player" sprite False startPosition (0, 0) ()

movePlayerRight :: Modifiers -> Position -> SpaceWarAction ()
movePlayerRight _ _ = do
    obj <- findObject "player" "playerGroup"
    (px, py) <- getObjectPosition obj
    (sx, sy) <- getObjectSize obj
     