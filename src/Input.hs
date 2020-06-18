module Input where

import FRPEngine.Input.Utils (vectorizeMovement)
import FRPEngine.Input.Types
import SDL

keybinds =
  [ InpBtn4D
      ( V4
          (Btn KeycodeM False)
          (Btn KeycodeT False)
          (Btn KeycodeS False)
          (Btn KeycodeN False)
      ),
    InpCloseWindow False
  ]

moveKey keys = vectorizeMovement (keys !! 0)

quitKey keys = case (keys !! 1) of
  (InpCloseWindow True) -> True
  (InpCloseWindow False) -> False
  _ -> error "Quit key not implemented"
