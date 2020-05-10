{-# LANGUAGE Arrows #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Actor where

import Control.Lens
import Control.Monad.Trans.Reader
import Data.Coerce
import FRP.Yampa
import FRPEngine.Input.Types
import FRPEngine.Input.Utils (vectorizeMovement)
import FRPEngine.Types
import FRPEngine.YampaUtils.Types ()
import Linear
import qualified Debug.Trace as Tr
import Control.Applicative

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

type IF a = ((RealFloat a) => (V2 a -> SF (V2 a) (V2 a)))

playerRun :: (RealFloat a, Show a) => Obj a _b -> Size a -> SF InputState (Obj a _b)
playerRun initObj origSize = proc input -> do
  (pos', size', vel') <- move (initObj ^. pos, initObj ^. size, initObj ^. vel, origSize) -< vectorizeMovement (input ^. movement)
  returnA -< (pos .~ pos') . (size .~ size') . (vel .~ vel') $ initObj

move :: forall a. (RealFloat a, Show a) => (Pos a, Size a, Vel a, Size a) -> SF (MoveKeys a) (Pos a, Size a, Vel a)
move (iPos, size, iVel, origSize) = proc dir -> do
  -- let moveXEnergy = case (0 < (dir ^. _x)) of
  --       -- Left button pressed
  --       True -> -1
  --       -- Right button pressed OR not pressing anything, return 0
  --       False -> 0
  --     moveYEnergy = (- (abs (dir ^. _y)))
  --     moveEnergy = sum $ V2 moveXEnergy moveYEnergy
  --     dir' = case (dir ^. _x) < 0 of
  --       True -> V2 0 (dir ^. _y)
  --       False -> dir
  -- Ok so here I want it to only gain force on velocity lost
  -- (energy, V1 energyToSpend) <- energyManagement (1, 0) -< V1 moveEnergy

  rec
    (dir'', energy) <- (iPre (0, 900) >>> moveVectorSF) -< Tr.trace ("Ok energy is: " ++ show energy) $ (dir, energy)
    (vel, breakEnergy) <- moveSF -< dir''
    -- vel <- integralFrom 0 -< dir'

  pos' <- integralFrom iPos -< vel

  -- Size
  size <- sizeRun origSize -< V2 -- (coerce energy *
                                 1000 (- (sum (abs vel)))
  returnA -<
    let pos = coerce <$> pos'
     in -- Tr.trace ("pos: " ++ show pos) $
    (pos, size, vel)
  where
    moveSF = velBreakingSF iVel $ moveForward integralFrom

-- * Energy

-- ** Energy

energyManagement :: (RealFloat a) => (V1 a, V1 a) -> SF (V1 a) (V1 a, V1 a)
energyManagement (_iEnergy, iDiff) =
  switch
    (sf _iEnergy)
    energyManagement
    where
      sf :: (RealFloat a) => V1 a -> SF (V1 a) ((V1 a, V1 a), Event (V1 a, V1 a))
      sf iEnergy = proc energyDraw -> do
        energy <- integralFrom iEnergy -< energyDraw
        let usedEnergy' =
              if energyDraw == 0
                then 0
                else energy / energyDraw
        let usedEnergy = if usedEnergy' > 1 then 1 else usedEnergy'
        returnA -<
          ( (energy, usedEnergy),
            if energy < 0 then Event (0, usedEnergy) else NoEvent
          )

-- * Movement

-- ** Break

type DeltaV a = V2 a
type Energy a = V2 a

type Dir a = V2 a
type Dir' a = V2 a

numToV2 a =
  if a == 0
  then V2 0 0
  else (V2 (a / 2) (a / 2))

moveVectorSF :: (RealFloat a, Show a) => SF (Dir a, a) (Dir' a, a)
moveVectorSF = proc (dir, energy') -> do
  let
    dt = 1
    -- -- This is dir but it's higher the slower you are
    -- dir' = liftA2 (\ vel dir ->
    --                    -- Fix div by zero error
    --                    let test = (if abs vel > 0 then (dir / vel) * dt else (dir / 0.0001) * dt)
    --                    -- Cap it
    --                    in if test > 100 then 100 else test
    --                 ) vel dir
    dir' = dir

    -- This is really dir goal
    dir'Cost = sum $ fmap abs dir'
    dir'MoneyLeftIFDebtable = (energy' - dir'Cost)
    -- How much I can afford
    dir'DirLeft = dir'MoneyLeft + dir'Cost
    dir'PurchaseFraction = dir'DirLeft / dir'Cost
    dir'MoneyLeft = if dir'DirLeft /= dir'Cost then 0 else dir'MoneyLeftIFDebtable

    dir'Product = dir' ^* dir'PurchaseFraction

      -- (((numToV2 energy') - dir') + dir')

    -- Calculate how much velocity and how much energy we have now
    (dir'V, dir'E) =
      case dir ^. _x < 0 of
        False -> (dir'Product, dir'MoneyLeft)
        True ->
          (0, energy')

  returnA -< (dir'V, dir'E)

-- Outputs (CurrVel, Ener)
velBreakingSF :: forall a. (RealFloat a) => V2 a -> IF a -> SF (MoveKeys a) (Vel a, a)
velBreakingSF _vel integralFunction =
  dSwitch
    (iPre 0 >>> sf _vel)
    (\(_vel', _newEnergy) -> (velBreakingSF _vel' integralFunction))
  where
    -- sf :: V2 a -> SF (V2 a) ((V2 a, a), Event (V2 a, V1 a))
    sf iVel = proc dir -> do

      dt <- constM getDt -< ()
      let
        dt' :: a
        dt' = realToFrac dt

      vel' <- integralFunction 0 -< dir
      returnA -<
          ( (vel', 0),
            ( case dir ^. _x < 0 of
                True -> Event (newVel, vel' - newVel)
                       where newVel = vel' / (1 + (20 ^* dt'))
                             friction = V1 $ sum $ abs (newVel / 2)

                False -> NoEvent
            )
          )

getDt :: ReaderT Double Identity Double
getDt = ask

-- ** Don't move back

moveForward integralFunction _vel =
  switch
    (sf _vel)
    (moveForward integralFunction)
  where
    sf iVel = proc dir -> do
      vel <- integralFunction iVel -< (dir * speed)
      returnA -< (vel,
                  if (vel ^. _x) < xMove
                  then Event (V2 xMove (vel ^. _y))
                  else NoEvent)

-- * Size

sizeRun :: (RealFloat a) => Size a -> SF (V2 a) (Size a)
sizeRun origSize = proc vel -> do
  let vel' = (V2 (vel ^. _x) (vel ^. _y))
  returnA -< (origSize / 2) + (keepMinimumSize <$> ((origSize / 2) + (vel' * sizeSpeed)))
  where
    keepMinimumSize a = if a < 0 then 0 else a
