{-|

Module      : SDL.Image
Copyright   : (c) 2015 Siniša Biđin
License     : MIT
Maintainer  : sinisa@bidin.eu
Stability   : experimental

Bindings to the @SDL2_image@ library. These should allow you to load various
types of images as @SDL@ 'Surface's, as well as detect image formats.

You can safely assume that any monadic function listed here is capable of
throwing an 'SDLException' in case it encounters an error.

-}

{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module SDL.Image
  (

  -- * Loading images
  --
  -- | Use the following functions to read any @PNG@, @JPG@, @TIF@, @GIF@,
  -- @WEBP@, @CUR@, @ICO@, @BMP@, @PNM@, @XPM@, @XCF@, @PCX@ and @XV@ formatted
  -- data.
  --
  -- If you have @TGA@-formatted data, you might wish to use the functions from
  -- the <#tga following section> instead.
    load
  , decode
  , loadTexture
  , decodeTexture

  -- * Loading TGA images
  --
  -- | #tga# Since @TGA@ images don't contain a specific unique signature, the
  -- following functions might succeed even when given files not formatted as
  -- @TGA@ images.
  --
  -- Only use these functions if you're certain the inputs are @TGA@-formatted,
  -- otherwise they'll throw an exception.
  , loadTGA
  , decodeTGA
  , loadTextureTGA
  , decodeTextureTGA

  -- * Format detection
  , formattedAs
  , format
  , Format(..)

   -- * Other
  , initialize
  , InitFlag(..)
  , version
  , quit
  ) where

import Control.Exception      (bracket, throwIO)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Bits              ((.|.))
import Data.ByteString        (ByteString)
import Data.ByteString.Unsafe (unsafeUseAsCStringLen)
import Data.List              (find)
import Data.Text              (pack)
import Foreign.C.String       (withCString)
import Foreign.C.Types        (CInt)
import Foreign.Ptr            (Ptr, castPtr)
import Foreign.Storable       (peek)
import GHC.Generics           (Generic)
import SDL                    (Renderer, Texture, Surface(..), SDLException(..))
import SDL.Internal.Exception (throwIfNull, throwIf_)
import SDL.Raw.Filesystem     (rwFromFile, rwFromConstMem)
import SDL.Raw.Types          (RWops)
import System.IO.Unsafe       (unsafePerformIO)

import qualified SDL
import qualified SDL.Raw
import qualified SDL.Raw.Image

-- | Initializes @SDL2_image@ by loading support for the chosen image formats.
-- Explicit initialization is optional.
--
-- You should call this function if you prefer to load image support yourself,
-- at a time when your process isn't as busy. Otherwise, image support will be
-- loaded dynamically when you attempt to load a @JPG@, @PNG@, @TIF@ or
-- @WEBP@-formatted file.
--
-- You may call this function multiple times.
initialize :: (Foldable f, MonadIO m) => f InitFlag -> m ()
initialize flags = do
  let cint = foldl (\a b -> a .|. flagToCInt b) 0 flags
  throwIf_
    (\result -> cint /= 0 && cint /= result)
    "SDL.Image.initialize"
    "IMG_Init"
    (SDL.Raw.Image.init cint)

-- | Flags intended to be fed to 'initialize'.
--
-- Each designates early loading of support for a particular image format.
data InitFlag
  = InitJPG  -- ^ Load support for reading @JPG@ files.
  | InitPNG  -- ^ Same, but for @PNG@ files.
  | InitTIF  -- ^ @TIF@ files.
  | InitWEBP -- ^ @WEBP@ files.
  deriving (Eq, Enum, Ord, Bounded, Generic, Read, Show)

flagToCInt :: InitFlag -> CInt
flagToCInt =
  \case
    InitJPG  -> SDL.Raw.Image.IMG_INIT_JPG
    InitPNG  -> SDL.Raw.Image.IMG_INIT_PNG
    InitTIF  -> SDL.Raw.Image.IMG_INIT_TIF
    InitWEBP -> SDL.Raw.Image.IMG_INIT_WEBP

-- | A helper for unmanaged 'Surface's, since it is not exposed by SDL itself.
unmanaged :: Ptr SDL.Raw.Surface -> Surface
unmanaged p = Surface p Nothing

-- | Loads any given file of a supported image type as a 'Surface', including
-- @TGA@ if the filename ends with @\".tga\"@.
--
-- If you have @TGA@ files that don't have names ending with @\".tga\"@, use
-- 'loadTGA' instead.
load :: MonadIO m => FilePath -> m Surface
load path =
  fmap unmanaged .
    throwIfNull "SDL.Image.load" "IMG_Load" .
      liftIO $ withCString path SDL.Raw.Image.load

-- | Same as 'load', but returning a 'Texture' instead.
--
-- For @TGA@ files not ending in ".tga", use 'loadTextureTGA' instead.
loadTexture :: MonadIO m => Renderer -> FilePath -> m Texture
loadTexture r path =
  liftIO . bracket (load path) SDL.freeSurface $
    SDL.createTextureFromSurface r

-- | Reads an image from a 'ByteString'.
--
-- This will work for all supported image types, __except TGA__. If you need to
-- decode a @TGA@ 'ByteString', use 'decodeTGA' instead.
decode :: MonadIO m => ByteString -> m Surface
decode bytes = liftIO .
  unsafeUseAsCStringLen bytes $ \(cstr, len) -> do
    rw <- rwFromConstMem (castPtr cstr) (fromIntegral len)
    fmap unmanaged .
      throwIfNull "SDL.Image.decode" "IMG_Load_RW" $
        SDL.Raw.Image.load_RW rw 0

-- | Same as 'decode', but returning a 'Texture' instead.
--
-- If you need to decode a @TGA@ 'ByteString', use 'decodeTextureTGA' instead.
decodeTexture :: MonadIO m => Renderer -> ByteString -> m Texture
decodeTexture r bytes =
  liftIO . bracket (decode bytes) SDL.freeSurface $
    SDL.createTextureFromSurface r

-- | If your @TGA@ files aren't in a filename ending with @\".tga\"@, you can
-- load them using this function.
loadTGA :: MonadIO m => FilePath -> m Surface
loadTGA path =
  fmap unmanaged .
    throwIfNull "SDL.Image.loadTGA" "IMG_LoadTGA_RW" .
      liftIO $ do
        rw <- withCString "rb" $ withCString path . flip rwFromFile
        SDL.Raw.Image.loadTGA_RW rw

-- | Same as 'loadTGA', only returning a 'Texture' instead.
loadTextureTGA :: MonadIO m => Renderer -> FilePath -> m Texture
loadTextureTGA r path =
  liftIO . bracket (loadTGA path) SDL.freeSurface $
    SDL.createTextureFromSurface r

-- | Reads a @TGA@ image from a 'ByteString'.
--
-- Assumes the input is a @TGA@-formatted image.
decodeTGA :: MonadIO m => ByteString -> m Surface
decodeTGA bytes = liftIO .
  unsafeUseAsCStringLen bytes $ \(cstr, len) -> do
    rw <- rwFromConstMem (castPtr cstr) (fromIntegral len)
    fmap unmanaged .
      throwIfNull "SDL.Image.decodeTGA" "IMG_LoadTGA_RW" $
        SDL.Raw.Image.loadTGA_RW rw

-- | Same as 'decodeTGA', but returns a 'Texture' instead.
decodeTextureTGA :: MonadIO m => Renderer -> ByteString -> m Texture
decodeTextureTGA r bytes =
  liftIO . bracket (decodeTGA bytes) SDL.freeSurface $
    SDL.createTextureFromSurface r

-- | Tests whether a 'ByteString' contains an image of a given format.
formattedAs :: Format -> ByteString -> Bool
formattedAs f bytes = unsafePerformIO .
  unsafeUseAsCStringLen bytes $ \(cstr, len) -> do
    rw <- rwFromConstMem (castPtr cstr) (fromIntegral len)
    formatPredicate f rw >>= \case
      1 -> return True
      0 -> return False
      e -> do
        let err = "Expected 1 or 0, got " `mappend` pack (show e) `mappend` "."
        let fun = "IMG_is" `mappend` pack (show f)
        throwIO $ SDLCallFailed "SDL.Image.formattedAs" fun err

-- | Tries to detect the image format by attempting 'formattedAs' with each
-- possible 'Format'.
--
-- If you're trying to test for a specific format, use a specific 'formattedAs'
-- directly instead.
format :: ByteString -> Maybe Format
format bytes = fst <$> find snd attempts
  where
    attempts = map (\f -> (f, formattedAs f bytes)) [minBound..]

-- | Each of the supported image formats.
data Format
  = CUR
  | ICO
  | BMP
  | PNM
  | XPM
  | XCF
  | PCX
  | GIF
  | LBM
  | XV
  | JPG
  | PNG
  | TIF
  | WEBP
  deriving (Eq, Enum, Ord, Bounded, Generic, Read, Show)

-- Given an image format, return its raw predicate function.
formatPredicate :: MonadIO m => Format -> Ptr RWops -> m CInt
formatPredicate = \case
  CUR  -> SDL.Raw.Image.isCUR
  ICO  -> SDL.Raw.Image.isICO
  BMP  -> SDL.Raw.Image.isBMP
  PNM  -> SDL.Raw.Image.isPNM
  XPM  -> SDL.Raw.Image.isXPM
  XCF  -> SDL.Raw.Image.isXCF
  PCX  -> SDL.Raw.Image.isPCX
  GIF  -> SDL.Raw.Image.isGIF
  LBM  -> SDL.Raw.Image.isLBM
  XV   -> SDL.Raw.Image.isXV
  JPG  -> SDL.Raw.Image.isJPG
  PNG  -> SDL.Raw.Image.isPNG
  TIF  -> SDL.Raw.Image.isTIF
  WEBP -> SDL.Raw.Image.isWEBP

-- | Gets the major, minor, patch versions of the linked @SDL2_image@ library.
version :: (Integral a, MonadIO m) => m (a, a, a)
version = liftIO $ do
  SDL.Raw.Version major minor patch <- peek =<< SDL.Raw.Image.getVersion
  return (fromIntegral major, fromIntegral minor, fromIntegral patch)

-- | Cleans up any loaded image libraries, freeing memory. You only need to
-- call this function once.
quit :: MonadIO m => m ()
quit = SDL.Raw.Image.quit
