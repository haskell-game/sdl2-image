{-|

Module      : SDL.Image
Description : High-level bindings.
Copyright   : (c) 2015 Siniša Biđin
License     : MIT
Maintainer  : sinisa@bidin.cc
Stability   : experimental
Portability : GHC

High-level bindings to the SDL_image library.

-}

{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module SDL.Image
  ( initialize
  , InitFlag(..)
  , load
  , loadTexture
  , loadTGA
  , loadTextureTGA
  , version
  , quit
  ) where

import Control.Exception      (bracket)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Bits              ((.|.))
import Data.Data              (Data)
import Data.Foldable          (Foldable, foldl)
import Data.Typeable          (Typeable)
import Foreign.C.String       (withCString)
import Foreign.C.Types        (CInt)
import Foreign.Storable       (peek)
import GHC.Generics           (Generic)
import Prelude         hiding (foldl)
import SDL                    (Renderer, Texture, Surface)
import SDL.Exception          (throwIfNull, throwIf)
import SDL.Raw.Filesystem     (rwFromFile)

import qualified SDL
import qualified SDL.Raw
import qualified SDL.Image.Raw as IMG

-- | Initializes `SDL_image` by loading support for the chosen image formats.
--
-- You should call this function if you prefer to load image support yourself,
-- at a time when your process isn't as busy. Otherwise, image support will be
-- loaded dynamically when you attempt to load a `JPG`, `PNG`, `TIF` or
-- `WEBP`-formatted file.
--
-- You may call this function multiple times.
initialize :: (Foldable f, MonadIO m) => f InitFlag -> m ()
initialize flags = do
  let cint = foldl (\a b -> a .|. flagToCInt b) 0 flags
  _ <- throwIf
    (\result -> cint /= 0 && cint /= result)
    "SDL.Image.initialize"
    "IMG_Init"
    (IMG.init cint)
  return ()

-- | Flags intended to be fed to 'initialize'. Each designates early loading of
-- support for a particular image format.
data InitFlag
  = InitJPG  -- ^ Load support for reading `JPG` files.
  | InitPNG  -- ^ Same, but for `PNG` files.
  | InitTIF  -- ^ `TIF` files.
  | InitWEBP -- ^ `WEBP` files.
  deriving (Eq, Enum, Ord, Bounded, Data, Generic, Typeable, Read, Show)

-- TODO: Use hsc2hs to fetch typedef enum from header file.
flagToCInt :: InitFlag -> CInt
flagToCInt =
  \case
    InitJPG  -> 1
    InitPNG  -> 2
    InitTIF  -> 4
    InitWEBP -> 8

-- | Loads any given file of a supported image type as a 'Surface', including
-- `TGA` if the filename ends with ".tga".
load :: (Functor m, MonadIO m) => FilePath -> m Surface
load path = do
  fmap SDL.Surface .
    throwIfNull "SDL.Image.load" "IMG_Load" .
      liftIO $ withCString path IMG.load

-- | Same as 'load', but returning a 'Texture' instead.
loadTexture :: (Functor m, MonadIO m) => Renderer -> FilePath -> m Texture
loadTexture r path =
  liftIO . bracket (load path) SDL.freeSurface $
    SDL.createTextureFromSurface r

-- | If your `TGA` files aren't in a filename ending with ".tga", you can use
-- this function instead. Note: since `TGA` files don't contain a specific
-- signature within them, this function might succeed in reading files of other
-- formats. Only use this function on files you know are `TGA`-formatted.
loadTGA :: (Functor m, MonadIO m) => FilePath -> m Surface
loadTGA path =
  fmap SDL.Surface .
    throwIfNull "SDL.Image.loadTGA" "IMG_LoadTGA_RW" .
      liftIO $ do
        rw <- withCString "rb" $ withCString path . flip rwFromFile
        IMG.loadTGA_RW rw

-- | Same as 'loadTGA', only returning a 'Texture' instead.
loadTextureTGA :: (Functor m, MonadIO m) => Renderer -> FilePath -> m Texture
loadTextureTGA r path =
  liftIO . bracket (loadTGA path) SDL.freeSurface $
    SDL.createTextureFromSurface r

-- | Gets the major, minor, patch versions of the linked `SDL_image` library.
version :: (Integral a, MonadIO m) => m (a, a, a)
version = liftIO $ do
  SDL.Raw.Version major minor patch <- peek =<< IMG.getVersion
  return (fromIntegral major, fromIntegral minor, fromIntegral patch)

-- | Clean up any loaded image libraries, freeing memory. You only need to call
-- this function once.
quit :: MonadIO m => m ()
quit = IMG.quit
