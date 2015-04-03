module SDL.Image.Raw where

#include "SDL_image.h"

import Prelude hiding (init)
import Data.Word (Word32)
import Foreign.Ptr (Ptr)
import Foreign.C.Types (CInt(..))
import Foreign.C.String (CString)
import Control.Monad.IO.Class (MonadIO, liftIO)
import SDL.Raw.Types (Version, Surface, RWops)

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

foreign import ccall "SDL_image.h IMG_Load_RW"
  load_RW' :: Ptr RWops -> CInt -> IO (Ptr Surface)

{-# INLINE load_RW #-}
load_RW :: MonadIO m => Ptr RWops -> CInt -> m (Ptr Surface)
load_RW src free = liftIO $ load_RW' src free

foreign import ccall "SDL_image.h IMG_Load"
  load' :: CString -> IO (Ptr Surface)

{-# INLINE load #-}
load :: MonadIO m => CString -> m (Ptr Surface)
load = liftIO . load'

foreign import ccall "SDL_image.h SDL_GetError"
  getError' :: IO CString

{-# INLINE getError #-}
getError :: MonadIO m => m CString
getError = liftIO getError'
