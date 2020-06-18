{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE Arrows #-}

module Actor where

import Control.Lens
import FRP.Yampa
import FRPEngine.Input.Utils (vectorizeMovement)
import FRPEngine.Input.Types
import FRPEngine.Types
import FRPEngine.Yampa.Types ()
import Linear
import Data.Coerce
import Control.Monad.Trans.Reader
import Types
import Input

speed :: (Num a) => V2 a
speed = V2 1000 500

sizeSpeed :: (RealFrac a) => V2 a
sizeSpeed = 0.05

type MoveKeys a = V2 a

type Pos a = V2 a
type Size a = V2 a
type Vel a = V2 a

xMove :: (Number a) => a
xMove = 50

maxSpeed :: (Number a) => a
maxSpeed = 2000

-- TODO: Very ugly code ahead, I would clean it up if I didn't just use this project to learn more about switching
move :: (Number a) => (Pos a, Size a, Vel a, Size a, a) -> SF (MoveKeys a) (Pos a, Size a, Vel a, a)
move (iP, size, a, origSize, fuel) = proc dir -> do
  (vel, fuel') <- velLimit (0, fuel) -< dir
  pos <- integralFrom iP -< vel
  size' <- sizeRun origSize fuel -< (vel, fuel')
  returnA -< (pos, size', coerce vel, fuel')
  where
    velLimit :: (Number a) => ((V2 a), a) -> SF (V2 a) (V2 a, a)
    velLimit (iVel, fuel) =
      switch
        sf
        (\a -> constant a)
      where sf = proc dir -> do
              (vel, fuel') <- breakVel (iVel, fuel) -< dir
              returnA -< (if sum (abs vel) > maxSpeed
                         then (vel * 0.99, fuel')
                         else (vel, fuel'), if fuel' < 0 then Event ((V2 (vel ^. _x) (-200)), 0) else NoEvent)

    sizeRun :: (Number a) => Size a -> a -> SF (V2 a, a) (Size a)
    sizeRun origSize origFuel = proc (vel, fuel) -> do
      let vel' = (V2 (vel ^. _x) ((vel ^. _y) - (vel ^. _x)))
      -- (keepMinimumSize <$> ((origSize / 2) + (vel' * sizeSpeed)))
      returnA -< (V2 (20 + (keepMinimumSize (fuel / 15))) ((origSize ^. _x / 2) + (keepMinimumSize (getY origSize / 2) - (getX vel' * getX sizeSpeed))))
        where
          getX a = (a ^. _x)
          getY = (^. _y)
          keepMinimumSize a = if a < 0 then 0 else a

    breakVel :: (Number a) => (V2 a, a) -> SF (V2 a) (V2 a, a)
    breakVel (iVel, fuel) =
      switch
        (iPre (V2 0 0) >>> sf)
        breakVel
      where sf = proc dir -> do
              dt <- constM ask -< ()
              let
                dt' :: (Number a) => a
                dt' = realToFrac dt

              vel' <- integralFrom iVel -< dir
              returnA -< ((vel', fuel - 1),
                                      case dir == 0 of
                                         True -> NoEvent
                                         False ->
                                           let vel'' = newVel vel' dir dt'
                                               delta = vel'' - vel'
                                           in Event (vel'',
                                                     let delta' = _y .~ abs (delta ^. _y) / 10 $ delta
                                                     in fuel - sum delta'))

                where
                  newVel v dir dt = (V2 (case dir ^. _x == 0 of
                                  True -> v ^._x
                                  False -> case dir ^. _x < 0 of
                                    -- Left
                                    True -> v ^. _x - ((v ^. _x) * 10 * dt)
                                    -- Right
                                    False -> case (abs v ^. _x) < 500 of
                                          True -> maxSpeed / (2 + dt)
                                          False -> v ^. _x + (10 * dt))
                           (10 * (dir ^. _y) + (v ^. _y)))

flipNumber limit num = sum limit - num

-- TODO: Very ugly unwrapping and wrapping
playerRun :: (Number a) => Player a _b -> SF [Input] ((Player a _b), a)
playerRun player@(Player (StretchCollObj origSize (CollObj _coll (Obj iPos iVel _rot iSize _spr _center))) iFuel) = proc input -> do
  (pos', size', vel', velCap') <- move (iPos, iSize, iVel, origSize, iFuel) -< moveKey input
  returnA -< (((objLens . pos) .~ pos') . ((objLens . size) .~ size') . ((objLens . vel) .~ vel') $ player, velCap')
    where objLens = pColl . collObj . obj
