module SDL.Image.Raw where

import Prelude hiding (init)
import Foreign.Ptr (Ptr)
import Foreign.C.Types (CInt(..))
import Foreign.C.String (CString)
import Control.Monad.IO.Class (MonadIO, liftIO)
import SDL.Raw.Types (Version, Surface, RWops)

foreign import ccall "SDL_image.h IMG_Linked_Version"
  getVersion' :: IO (Ptr Version)

{-# INLINE getVersion #-}
{-| const SDL_version *IMG_Linked_Version() -}
getVersion :: MonadIO m => m (Ptr Version)
getVersion = liftIO getVersion'

type InitFlags = CInt

foreign import ccall "SDL_image.h IMG_Init"
  init' :: InitFlags -> IO InitFlags

{-# INLINE init #-}
{-| int IMG_Init(int flags) -}
init :: MonadIO m => InitFlags -> m InitFlags
init = liftIO . init'

foreign import ccall "SDL_image.h IMG_Quit"
  quit' :: IO ()

{-# INLINE quit #-}
{-| void IMG_Quit() -}
quit :: MonadIO m => m ()
quit = liftIO quit'

foreign import ccall "SDL_image.h IMG_Load"
  load' :: CString -> IO (Ptr Surface)

{-# INLINE load #-}
{-| SDL_Surface *IMG_Load(const char *file) -}
load :: MonadIO m => CString -> m (Ptr Surface)
load = liftIO . load'

foreign import ccall "SDL_image.h IMG_Load_RW"
  load_RW' :: Ptr RWops -> CInt -> IO (Ptr Surface)

type Free = CInt

{-# INLINE load_RW #-}
{-| SDL_Surface *IMG_Load_RW(SDL_RWops *src, int freesrc) -}
load_RW :: MonadIO m => Ptr RWops -> Free -> m (Ptr Surface)
load_RW src free = liftIO $ load_RW' src free

foreign import ccall "SDL_image.h SDL_GetError"
  getError' :: IO CString

{-# INLINE getError #-}
{-| char *SDL_GetError() -}
getError :: MonadIO m => m CString
getError = liftIO getError'
