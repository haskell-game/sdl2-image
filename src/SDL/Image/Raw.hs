module SDL.Image.Raw where

import Foreign.Ptr (Ptr)
import Control.Monad.IO.Class (MonadIO, liftIO)
import SDL.Raw.Types (Version)

foreign import ccall "SDL_image.h IMG_Linked_Version"
  getVersion' :: IO (Ptr Version)

{-# INLINE getVersion #-}
getVersion :: MonadIO m => m (Ptr Version)
getVersion = liftIO getVersion'
