{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Types where

import Control.Lens
import SDL as S
import SDL.Font (Font)
import FRPEngine.Types
import Data.Aeson
import GHC.Generics

data Resources
  = Resources
      { _font :: Font,
        _objectSprite :: S.Texture,
        _objectSprite2 :: S.Texture
      }

makeLenses ''Resources

data SpriteSelect
  = Sfont
  | SobjectSprite
  | SobjectSprite2
  deriving (Generic, Show)

getSprite :: Obj a SpriteSelect -> Resources -> S.Texture
getSprite obj =
  case (obj ^. spr) of
    SobjectSprite -> _objectSprite
    SobjectSprite2 -> _objectSprite2

data CameraState
  = CameraState
      { _zoomLevel :: Int
      }
  deriving (Generic, Show)

data StretchCollObj a spriteSelect =
  StretchCollObj
  {
    _origSize :: V2 a,
    _collObj :: CollObj a spriteSelect
  }
  deriving(Generic, Show)

makeLenses ''StretchCollObj

data Player a spriteSelect =
  Player
  {
    _pColl :: StretchCollObj a SpriteSelect,
    _fuel :: a
  }
  deriving (Generic, Show)

makeLenses ''Player

data PhysicalState
  = PhysicalState
  {
    _player :: Player Double SpriteSelect,
    _enemies :: [StretchCollObj Double SpriteSelect]
  }
  deriving (Generic, Show)

makeLenses ''PhysicalState

data GameState
  = GameState
      { _cameraState :: CameraState,
        _physicalState :: PhysicalState,
        _alive :: Bool
      }
  deriving (Generic, Show)

makeLenses ''GameState

instance FromJSON GameState
instance FromJSON CameraState
instance FromJSON PhysicalState
instance FromJSON SpriteSelect
instance (FromJSON a, FromJSON spriteSelect) => FromJSON (Player a spriteSelect)
instance (FromJSON a, FromJSON spriteSelect) => FromJSON (StretchCollObj a spriteSelect)


instance ToJSON GameState
instance ToJSON CameraState
instance ToJSON PhysicalState
instance ToJSON SpriteSelect
instance (ToJSON a, ToJSON spriteSelect) => ToJSON (Player a spriteSelect)
instance (ToJSON a, ToJSON spriteSelect) => ToJSON (StretchCollObj a spriteSelect)
