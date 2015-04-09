{-|

Module      : SDL.Image
Description : High-level bindings.
Copyright   : (c) 2015 Siniša Biđin
License     : MIT
Maintainer  : sinisa@bidin.cc
Stability   : experimental

High-level bindings to the SDL_image library.

-}

{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module SDL.Image
  (

  -- * Loading most images
  --
  -- | Use the following actions to read any @PNG@, @JPG@, @TIF@, @GIF@,
  -- @WEBP@, @CUR@, @ICO@, @BMP@, @PNM@, @XPM@, @XCF@, @PCX@ and @XV@ formatted
  -- data. If you have @TGA@-formatted data, you might wish to use the actions
  -- from the <#tga following section> instead.
    load
  , decode
  , loadTexture
  , decodeTexture

  -- * Loading TGA images
  --
  -- | #tga# Since @TGA@ images don't contain a specific unique signature, the
  -- following actions might succeed even when given files not formatted as
  -- @TGA@ images. Only use these actions if you're certain the inputs are
  -- @TGA@-formatted, otherwise they'll throw an exception.
  , loadTGA
  , decodeTGA
  , loadTextureTGA
  , decodeTextureTGA

   -- * Other
  , initialize
  , InitFlag(..)
  , version
  , quit
  ) where

import Control.Exception      (bracket)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Bits              ((.|.))
import Data.ByteString        (ByteString)
import Data.ByteString.Unsafe (unsafeUseAsCStringLen)
import Data.Data              (Data)
import Data.Typeable          (Typeable)
import Foreign.C.String       (withCString)
import Foreign.C.Types        (CInt)
import Foreign.Ptr            (castPtr)
import Foreign.Storable       (peek)
import GHC.Generics           (Generic)
import SDL                    (Renderer, Texture, Surface)
import SDL.Exception          (throwIfNull, throwIf)
import SDL.Raw.Filesystem     (rwFromFile, rwFromConstMem)

import qualified SDL
import qualified SDL.Raw
import qualified SDL.Raw.Image

-- | Initializes @SDL_image@ by loading support for the chosen image formats.
-- Explicit initialization is optional.
--
-- You should call this action if you prefer to load image support yourself,
-- at a time when your process isn't as busy. Otherwise, image support will be
-- loaded dynamically when you attempt to load a @JPG@, @PNG@, @TIF@ or
-- @WEBP@-formatted file.
--
-- You may call this action multiple times.
initialize :: (Foldable f, MonadIO m) => f InitFlag -> m ()
initialize flags = do
  let cint = foldl (\a b -> a .|. flagToCInt b) 0 flags
  _ <- throwIf
    (\result -> cint /= 0 && cint /= result)
    "SDL.Image.initialize"
    "IMG_Init"
    (SDL.Raw.Image.init cint)
  return ()

-- | Flags intended to be fed to 'initialize'. Each designates early loading of
-- support for a particular image format.
data InitFlag
  = InitJPG  -- ^ Load support for reading @JPG@ files.
  | InitPNG  -- ^ Same, but for @PNG@ files.
  | InitTIF  -- ^ @TIF@ files.
  | InitWEBP -- ^ @WEBP@ files.
  deriving (Eq, Enum, Ord, Bounded, Data, Generic, Typeable, Read, Show)

flagToCInt :: InitFlag -> CInt
flagToCInt =
  \case
    InitJPG  -> SDL.Raw.Image.IMG_INIT_JPG
    InitPNG  -> SDL.Raw.Image.IMG_INIT_PNG
    InitTIF  -> SDL.Raw.Image.IMG_INIT_TIF
    InitWEBP -> SDL.Raw.Image.IMG_INIT_WEBP

-- | Loads any given file of a supported image type as a 'Surface', including
-- @TGA@ if the filename ends with @\".tga\"@. If you have @TGA@ files that
-- don't have names ending with @\".tga\"@, use 'loadTGA' instead.
load :: (Functor m, MonadIO m) => FilePath -> m Surface
load path = do
  fmap SDL.Surface .
    throwIfNull "SDL.Image.load" "IMG_Load" .
      liftIO $ withCString path SDL.Raw.Image.load

-- | Same as 'load', but returning a 'Texture' instead. For @TGA@ files not
-- ending in ".tga", use 'loadTextureTGA' instead.
loadTexture :: (Functor m, MonadIO m) => Renderer -> FilePath -> m Texture
loadTexture r path =
  liftIO . bracket (load path) SDL.freeSurface $
    SDL.createTextureFromSurface r

-- | Reads an image from a 'ByteString'. This will work for all supported image
-- types, __except TGA__. If you need to decode a @TGA@ 'ByteString', use
-- 'decodeTGA' instead.
decode :: MonadIO m => ByteString -> m Surface
decode bytes = liftIO $ do
  unsafeUseAsCStringLen bytes $ \(cstr, len) -> do
    rw <- rwFromConstMem (castPtr cstr) (fromIntegral len)
    fmap SDL.Surface .
      throwIfNull "SDL.Image.decode" "IMG_Load_RW" $
        SDL.Raw.Image.load_RW rw 0

-- | Same as 'decode', but returning a 'Texture' instead. If you need to decode
-- a @TGA@ 'ByteString', use 'decodeTextureTGA' instead.
decodeTexture :: MonadIO m => Renderer -> ByteString -> m Texture
decodeTexture r bytes =
  liftIO . bracket (decode bytes) SDL.freeSurface $
    SDL.createTextureFromSurface r

-- | If your @TGA@ files aren't in a filename ending with @\".tga\"@, you can
-- load them using this action.
loadTGA :: (Functor m, MonadIO m) => FilePath -> m Surface
loadTGA path =
  fmap SDL.Surface .
    throwIfNull "SDL.Image.loadTGA" "IMG_LoadTGA_RW" .
      liftIO $ do
        rw <- withCString "rb" $ withCString path . flip rwFromFile
        SDL.Raw.Image.loadTGA_RW rw

-- | Same as 'loadTGA', only returning a 'Texture' instead.
loadTextureTGA :: (Functor m, MonadIO m) => Renderer -> FilePath -> m Texture
loadTextureTGA r path =
  liftIO . bracket (loadTGA path) SDL.freeSurface $
    SDL.createTextureFromSurface r

-- | Reads a @TGA@ image from a 'ByteString'. Assumes the input is a
-- @TGA@-formatted image.
decodeTGA :: MonadIO m => ByteString -> m Surface
decodeTGA bytes = liftIO $ do
  unsafeUseAsCStringLen bytes $ \(cstr, len) -> do
    rw <- rwFromConstMem (castPtr cstr) (fromIntegral len)
    fmap SDL.Surface .
      throwIfNull "SDL.Image.decodeTGA" "IMG_LoadTGA_RW" $
        SDL.Raw.Image.loadTGA_RW rw

-- | Same as 'decodeTGA', but returns a 'Texture' instead.
decodeTextureTGA :: MonadIO m => Renderer -> ByteString -> m Texture
decodeTextureTGA r bytes =
  liftIO . bracket (decodeTGA bytes) SDL.freeSurface $
    SDL.createTextureFromSurface r

-- | Gets the major, minor, patch versions of the linked @SDL_image@ library.
version :: (Integral a, MonadIO m) => m (a, a, a)
version = liftIO $ do
  SDL.Raw.Version major minor patch <- peek =<< SDL.Raw.Image.getVersion
  return (fromIntegral major, fromIntegral minor, fromIntegral patch)

-- | Cleans up any loaded image libraries, freeing memory. You only need to
-- call this action once.
quit :: MonadIO m => m ()
quit = SDL.Raw.Image.quit
