module SDL.Image.Raw where

#include "SDL_image.h"

import Prelude hiding (init)
import Data.Word (Word32)
import Foreign.Ptr (Ptr)
import Foreign.C.Types (CInt(..))
import Control.Monad.IO.Class (MonadIO, liftIO)
import SDL.Raw.Types (Version)

foreign import ccall "SDL_image.h IMG_Linked_Version"
  getVersion' :: IO (Ptr Version)

{-# INLINE getVersion #-}
getVersion :: MonadIO m => m (Ptr Version)
getVersion = liftIO getVersion'

type InitFlags = Word32

foreign import ccall "SDL_image.h IMG_Init"
  init' :: InitFlags -> IO CInt

{-# INLINE init #-}
init :: MonadIO m => InitFlags -> m CInt
init = liftIO . init'

foreign import ccall "SDL_image.h IMG_Quit"
  quit' :: IO ()

{-# INLINE quit #-}
quit :: MonadIO m => m ()
quit = liftIO quit'
