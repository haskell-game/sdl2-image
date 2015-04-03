{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module SDL.Image where

import Data.Word (Word32)
import Data.Data (Data)
import Data.Text (Text)
import Control.Applicative ((<$>))
import Control.Exception (bracket)
import Data.ByteString (packCString)
import Data.Text.Encoding (decodeUtf8)
import Data.Foldable (Foldable)
import Data.Typeable (Typeable)
import Control.Monad.IO.Class (MonadIO, liftIO)
import GHC.Generics (Generic)
import Foreign.Storable (peek)
import Foreign.C.String (withCString)
import SDL.Image.Internal.Bitmask (foldFlags)
import SDL.Image.Internal.Numbered (ToNumber(..))
import SDL (Renderer, Texture, Surface)

import qualified SDL
import qualified SDL.Raw as Raw
import qualified SDL.Image.Raw as IMG

-- | Gets the major, minor and patch versions of the linked SDL_image library.
-- Does not require initialization.
version :: (Integral a, MonadIO m) => m (a, a, a)
version = liftIO $ do
  Raw.Version major minor patch <- peek =<< IMG.getVersion
  return (fromIntegral major, fromIntegral minor, fromIntegral patch)

-- | Initializes SDL_image by loading support for the given image formats. You
-- should call this function if you prefer to load image support yourself, at a
-- time when the process isn't as busy. Otherwise, image support will be loaded
-- dynamically when you attempt to load a JPG, PNG, TIF or WEBP-formatted file.
-- You may call this function multiple times.
initialize :: (Foldable f, MonadIO m) => f InitFlag -> m ()
initialize flags = do
  _ <- IMG.init $ foldFlags toNumber flags -- TODO: Check for error.
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
quit = IMG.quit

-- | Loads any given supported image file as a Surface, including TGA if the
-- filename ends with ".tga".
load :: MonadIO m => FilePath -> m Surface
load path = liftIO . fmap SDL.pointerToSurface $ withCString path IMG.load

-- | Same as SDL.Image.load, but returning a Texture instead.
loadTexture :: (Functor m, MonadIO m) => Renderer -> FilePath -> m Texture
loadTexture r path =
  liftIO . bracket (load path) SDL.freeSurface $ \surface -> do
    SDL.createTextureFromSurface r surface

-- | Returns the last error string set by a previously called SDL or SDL_image
-- function. Is the same as SDL.getError.
getError :: MonadIO m => m Text
getError = liftIO $ do
  cstr <- IMG.getError
  decodeUtf8 <$> packCString cstr
