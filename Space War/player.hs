module Player(
    playerCreate,
    movePlayerRight,
    movePlayerLeft,
    movePlayerUp,
    movePlayerDown
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
    if (px + (sx/2) < width)
        then (setObjectPosition ((px + 7), py) obj)
        else (setObjectPosition (width - (sx/2), py) obj)

movePlayerLeft :: Modifiers -> Position -> SpaceWarAction ()
movePlayerLeft _ _ = do
    obj <- findObject "player" "playerGroup"
    (px, py) <- getObjectPosition obj
    (sx, sy) <- getObjectSize obj
    if(px - (sx/2) > 0)
        then (setObjectPosition ((px - 7), py) obj)
        else (setObjectPosition (width + (sx/2), py) obj)

movePlayerUp :: Modifiers -> Position -> SpaceWarAction ()
movePlayerUp _ _ = do
    obj <- findObject "player" "playerGroup"
    (px, py) <- getObjectPosition obj
    (sx, sy) <- getObjectSize obj
    if(py + (sy/2) < height)
        then (setObjectPosition (px, (py + 7)) obj)
        else (setObjectPosition (px, height - (sy/2)) obj)

movePlayerDown :: Modifiers -> Position -> SpaceWarAction ()
movePlayerDown _ _ = do
    obj <- findObject "player" "playerGroup"
    (px, py) <- getObjectPosition obj
    (sx, sy) <- getObjectSize obj
    if(py - (sy/2) > 0)
        then (setObjectPosition (px, (py - 7)) obj)
        else (setObjectPosition (px, (height + (sy/2))) obj)