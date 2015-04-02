{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module SDL.Image where

import Data.Word (Word32)
import Data.Data (Data)
import Data.Foldable (Foldable)
import Data.Typeable (Typeable)
import Control.Monad.IO.Class (MonadIO, liftIO)
import GHC.Generics (Generic)
import Foreign.Storable (peek)
import SDL.Image.Internal.Bitmask (foldFlags)
import SDL.Image.Internal.Numbered (ToNumber(..))

import qualified SDL.Raw as Sdl
import qualified SDL.Image.Raw as Img

-- | Gets the major, minor and patch versions of the linked SDL_image library.
-- Does not require initialization.
version :: (Integral a, MonadIO m) => m (a, a, a)
version = liftIO $ do
  Sdl.Version major minor patch <- peek =<< Img.getVersion
  return (fromIntegral major, fromIntegral minor, fromIntegral patch)

-- | Initializes SDL_image by loading support for the given image formats. Do
-- not call any SDL_image functions prior to this one, unless the documentation
-- states you may do otherwise. You may call this function multiple times.
initialize :: (Foldable f, MonadIO m) => f InitFlag -> m ()
initialize flags = do
  _ <- Img.init $ foldFlags toNumber flags -- TODO: Check for error.
  return ()

data InitFlag
  = InitJPG
  | InitPNG
  | InitTIF
  | InitWEBP
  deriving (Eq, Enum, Ord, Bounded, Data, Generic, Typeable, Read, Show)

-- TODO: Use hsc2hs to fetch typedef enum from header file.
instance ToNumber InitFlag Word32 where
  toNumber InitJPG  = 1
  toNumber InitPNG  = 2
  toNumber InitTIF  = 4
  toNumber InitWEBP = 8

-- | Clean up any loaded image libraries, freeing memory. You only need to call
-- this function once.
quit :: MonadIO m => m ()
quit = Img.quit
