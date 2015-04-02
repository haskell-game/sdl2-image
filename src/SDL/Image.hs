module SDL.Image where

import Control.Monad.IO.Class (MonadIO, liftIO)

import Foreign.Marshal.Alloc (alloca)
import Foreign.Storable (peek)

import qualified SDL.Raw as SdlRaw
import qualified SDL.Image.Raw as ImgRaw

version :: (Integral a, MonadIO m) => m (a, a, a)
version = liftIO $ do
  SdlRaw.Version major minor patch <- peek =<< ImgRaw.getVersion
  return (fromIntegral major, fromIntegral minor, fromIntegral patch)
