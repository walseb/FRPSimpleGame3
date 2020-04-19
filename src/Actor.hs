{-# LANGUAGE Arrows #-}

module Actor where

import Control.Lens
import Data.Coerce
import FRP.Yampa
import FRPEngine.Input.Utils (vectorizeMovement)
import FRPEngine.Input.Types
import FRPEngine.Types
import FRPEngine.YampaUtils.Types ()
import Linear

speed :: (Num a) => V2 a
speed = V2 1000 500

sizeSpeed :: (RealFrac a) => V2 a
sizeSpeed = 0.05

type MoveKeys a = V2 a

type Pos a = V2 a
type Size a = V2 a
type Vel a = V2 a

xMove :: (RealFloat a) => a
xMove = 50

move :: (RealFloat a) => (Pos a, Size a, Vel a, Size a) -> SF (MoveKeys a) (Pos a, Size a, Vel a)
move (V2 iPX iPY, size, a, origSize) = proc dir -> do
  -- Pos
  vel <- moveNoBackSwitch a -< dir
  posX' <- integralFrom (V1 iPX) -< V1 (vel ^. _x)
  posY <- integralFrom (V1 iPY) -< V1 (vel ^. _y)

  -- Size
  size <- sizeRun origSize -< vel

  returnA -<
    let pos = coerce <$> V2 posX' posY
     in (pos, size, vel)

moveVel :: (RealFloat a) => Vel a -> SF (MoveKeys a) (Vel a, Event (Vel a))
moveVel initVel = proc dir -> do
  vel <- integralFrom initVel -< (dir * speed)
  returnA -< (vel, if (vel ^. _x) < xMove then Event (V2 xMove (vel ^. _y)) else NoEvent)

moveNoBackSwitch :: (RealFloat a) => Vel a -> SF (MoveKeys a) (Vel a)
moveNoBackSwitch initVel =
  switch
    (moveVel initVel)
    moveNoBackSwitch

sizeRun :: (RealFloat a) => Size a -> SF (V2 a) (Size a)
sizeRun origSize = proc vel -> do
  let vel' = (V2 (vel ^. _x) ((vel ^. _y) - (vel ^. _x)))
  returnA -< (origSize / 2) + (keepMinimumSize <$> ((origSize / 2) + (vel' * sizeSpeed)))
    where
      keepMinimumSize a = if a < 0 then 0 else a

playerRun :: (RealFloat a) => Obj a _b -> Size a -> SF InputState (Obj a _b)
playerRun initObj origSize = proc input -> do
  (pos', size', vel') <- move (initObj ^. pos, initObj ^. size, initObj ^. vel, origSize) -< vectorizeMovement (input ^. movement)
  returnA -< (pos .~ pos') . (size .~ size') . (vel .~ vel') $ initObj
