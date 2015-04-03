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
load_RW src = liftIO . load_RW' src

type Format = CString

foreign import ccall "SDL_image.h IMG_LoadTyped_RW"
  loadTyped_RW' :: Ptr RWops -> Free -> Format -> IO (Ptr Surface)

{-# INLINE loadTyped_RW #-}
{-| SDL_Surface *IMG_LoadTyped_RW(SDL_RWops *src, int freesrc, char *type) -}
loadTyped_RW :: MonadIO m => Ptr RWops -> Free -> Format -> m (Ptr Surface)
loadTyped_RW src free = liftIO . loadTyped_RW' src free

foreign import ccall "SDL_image.h IMG_LoadCUR_RW"
  loadCUR_RW' :: Ptr RWops -> IO (Ptr Surface)

{-# INLINE loadCUR_RW #-}
{-| SDL_Surface *IMG_LoadCUR_RW(SDL_RWops *src) -}
loadCUR_RW :: MonadIO m => Ptr RWops -> m (Ptr Surface)
loadCUR_RW = liftIO . loadCUR_RW'

foreign import ccall "SDL_image.h IMG_LoadICO_RW"
  loadICO_RW' :: Ptr RWops -> IO (Ptr Surface)

{-# INLINE loadICO_RW #-}
{-| SDL_Surface *IMG_LoadICO_RW(SDL_RWops *src) -}
loadICO_RW :: MonadIO m => Ptr RWops -> m (Ptr Surface)
loadICO_RW = liftIO . loadICO_RW'

foreign import ccall "SDL_image.h IMG_LoadBMP_RW"
  loadBMP_RW' :: Ptr RWops -> IO (Ptr Surface)

{-# INLINE loadBMP_RW #-}
{-| SDL_Surface *IMG_LoadBMP_RW(SDL_RWops *src) -}
loadBMP_RW :: MonadIO m => Ptr RWops -> m (Ptr Surface)
loadBMP_RW = liftIO . loadBMP_RW'

foreign import ccall "SDL_image.h IMG_LoadPNM_RW"
  loadPNM_RW' :: Ptr RWops -> IO (Ptr Surface)

{-# INLINE loadPNM_RW #-}
{-| SDL_Surface *IMG_LoadPNM_RW(SDL_RWops *src) -}
loadPNM_RW :: MonadIO m => Ptr RWops -> m (Ptr Surface)
loadPNM_RW = liftIO . loadPNM_RW'

foreign import ccall "SDL_image.h IMG_LoadXPM_RW"
  loadXPM_RW' :: Ptr RWops -> IO (Ptr Surface)

{-# INLINE loadXPM_RW #-}
{-| SDL_Surface *IMG_LoadXPM_RW(SDL_RWops *src) -}
loadXPM_RW :: MonadIO m => Ptr RWops -> m (Ptr Surface)
loadXPM_RW = liftIO . loadXPM_RW'

foreign import ccall "SDL_image.h IMG_LoadXCF_RW"
  loadXCF_RW' :: Ptr RWops -> IO (Ptr Surface)

{-# INLINE loadXCF_RW #-}
{-| SDL_Surface *IMG_LoadXCF_RW(SDL_RWops *src) -}
loadXCF_RW :: MonadIO m => Ptr RWops -> m (Ptr Surface)
loadXCF_RW = liftIO . loadXCF_RW'

foreign import ccall "SDL_image.h IMG_LoadPCX_RW"
  loadPCX_RW' :: Ptr RWops -> IO (Ptr Surface)

{-# INLINE loadPCX_RW #-}
{-| SDL_Surface *IMG_LoadPCX_RW(SDL_RWops *src) -}
loadPCX_RW :: MonadIO m => Ptr RWops -> m (Ptr Surface)
loadPCX_RW = liftIO . loadPCX_RW'

foreign import ccall "SDL_image.h IMG_LoadGIF_RW"
  loadGIF_RW' :: Ptr RWops -> IO (Ptr Surface)

{-# INLINE loadGIF_RW #-}
{-| SDL_Surface *IMG_LoadGIF_RW(SDL_RWops *src) -}
loadGIF_RW :: MonadIO m => Ptr RWops -> m (Ptr Surface)
loadGIF_RW = liftIO . loadGIF_RW'

foreign import ccall "SDL_image.h IMG_LoadJPG_RW"
  loadJPG_RW' :: Ptr RWops -> IO (Ptr Surface)

{-# INLINE loadJPG_RW #-}
{-| SDL_Surface *IMG_LoadJPG_RW(SDL_RWops *src) -}
loadJPG_RW :: MonadIO m => Ptr RWops -> m (Ptr Surface)
loadJPG_RW = liftIO . loadJPG_RW'

foreign import ccall "SDL_image.h IMG_LoadTIF_RW"
  loadTIF_RW' :: Ptr RWops -> IO (Ptr Surface)

{-# INLINE loadTIF_RW #-}
{-| SDL_Surface *IMG_LoadTIF_RW(SDL_RWops *src) -}
loadTIF_RW :: MonadIO m => Ptr RWops -> m (Ptr Surface)
loadTIF_RW = liftIO . loadTIF_RW'

foreign import ccall "SDL_image.h IMG_LoadPNG_RW"
  loadPNG_RW' :: Ptr RWops -> IO (Ptr Surface)

{-# INLINE loadPNG_RW #-}
{-| SDL_Surface *IMG_LoadPNG_RW(SDL_RWops *src) -}
loadPNG_RW :: MonadIO m => Ptr RWops -> m (Ptr Surface)
loadPNG_RW = liftIO . loadPNG_RW'

foreign import ccall "SDL_image.h IMG_LoadTGA_RW"
  loadTGA_RW' :: Ptr RWops -> IO (Ptr Surface)

{-# INLINE loadTGA_RW #-}
{-| SDL_Surface *IMG_LoadTGA_RW(SDL_RWops *src) -}
loadTGA_RW :: MonadIO m => Ptr RWops -> m (Ptr Surface)
loadTGA_RW = liftIO . loadTGA_RW'

foreign import ccall "SDL_image.h IMG_LoadLBM_RW"
  loadLBM_RW' :: Ptr RWops -> IO (Ptr Surface)

{-# INLINE loadLBM_RW #-}
{-| SDL_Surface *IMG_LoadLBM_RW(SDL_RWops *src) -}
loadLBM_RW :: MonadIO m => Ptr RWops -> m (Ptr Surface)
loadLBM_RW = liftIO . loadLBM_RW'

foreign import ccall "SDL_image.h IMG_LoadXV_RW"
  loadXV_RW' :: Ptr RWops -> IO (Ptr Surface)

{-# INLINE loadXV_RW #-}
{-| SDL_Surface *IMG_LoadXV_RW(SDL_RWops *src) -}
loadXV_RW :: MonadIO m => Ptr RWops -> m (Ptr Surface)
loadXV_RW = liftIO . loadXV_RW'

foreign import ccall "SDL_image.hr IMG_isCUR"
  isCUR' :: Ptr RWops -> IO CInt

{-# INLINE isCUR #-}
{-| int IMG_isCUR(SDL_RWops *src) -}
isCUR :: MonadIO m => Ptr RWops -> m CInt
isCUR = liftIO . isCUR'

foreign import ccall "SDL_image.hr IMG_isICO"
  isICO' :: Ptr RWops -> IO CInt

{-# INLINE isICO #-}
{-| int IMG_isICO(SDL_RWops *src) -}
isICO :: MonadIO m => Ptr RWops -> m CInt
isICO = liftIO . isICO'

foreign import ccall "SDL_image.hr IMG_isBMP"
  isBMP' :: Ptr RWops -> IO CInt

{-# INLINE isBMP #-}
{-| int IMG_isBMP(SDL_RWops *src) -}
isBMP :: MonadIO m => Ptr RWops -> m CInt
isBMP = liftIO . isBMP'

foreign import ccall "SDL_image.hr IMG_isPNM"
  isPNM' :: Ptr RWops -> IO CInt

{-# INLINE isPNM #-}
{-| int IMG_isPNM(SDL_RWops *src) -}
isPNM :: MonadIO m => Ptr RWops -> m CInt
isPNM = liftIO . isPNM'

foreign import ccall "SDL_image.hr IMG_isXPM"
  isXPM' :: Ptr RWops -> IO CInt

{-# INLINE isXPM #-}
{-| int IMG_isXPM(SDL_RWops *src) -}
isXPM :: MonadIO m => Ptr RWops -> m CInt
isXPM = liftIO . isXPM'

foreign import ccall "SDL_image.hr IMG_isXCF"
  isXCF' :: Ptr RWops -> IO CInt

{-# INLINE isXCF #-}
{-| int IMG_isXCF(SDL_RWops *src) -}
isXCF :: MonadIO m => Ptr RWops -> m CInt
isXCF = liftIO . isXCF'

foreign import ccall "SDL_image.hr IMG_isPCX"
  isPCX' :: Ptr RWops -> IO CInt

{-# INLINE isPCX #-}
{-| int IMG_isPCX(SDL_RWops *src) -}
isPCX :: MonadIO m => Ptr RWops -> m CInt
isPCX = liftIO . isPCX'

foreign import ccall "SDL_image.hr IMG_isGIF"
  isGIF' :: Ptr RWops -> IO CInt

{-# INLINE isGIF #-}
{-| int IMG_isGIF(SDL_RWops *src) -}
isGIF :: MonadIO m => Ptr RWops -> m CInt
isGIF = liftIO . isGIF'

foreign import ccall "SDL_image.hr IMG_isJPG"
  isJPG' :: Ptr RWops -> IO CInt

{-# INLINE isJPG #-}
{-| int IMG_isJPG(SDL_RWops *src) -}
isJPG :: MonadIO m => Ptr RWops -> m CInt
isJPG = liftIO . isJPG'

foreign import ccall "SDL_image.hr IMG_isTIF"
  isTIF' :: Ptr RWops -> IO CInt

{-# INLINE isTIF #-}
{-| int IMG_isTIF(SDL_RWops *src) -}
isTIF :: MonadIO m => Ptr RWops -> m CInt
isTIF = liftIO . isTIF'

foreign import ccall "SDL_image.hr IMG_isPNG"
  isPNG' :: Ptr RWops -> IO CInt

{-# INLINE isPNG #-}
{-| int IMG_isPNG(SDL_RWops *src) -}
isPNG :: MonadIO m => Ptr RWops -> m CInt
isPNG = liftIO . isPNG'

foreign import ccall "SDL_image.hr IMG_isLBM"
  isLBM' :: Ptr RWops -> IO CInt

{-# INLINE isLBM #-}
{-| int IMG_isLBM(SDL_RWops *src) -}
isLBM :: MonadIO m => Ptr RWops -> m CInt
isLBM = liftIO . isLBM'

foreign import ccall "SDL_image.hr IMG_isXV"
  isXV' :: Ptr RWops -> IO CInt

{-# INLINE isXV #-}
{-| int IMG_isXV(SDL_RWops *src) -}
isXV :: MonadIO m => Ptr RWops -> m CInt
isXV = liftIO . isXV'
