{-|

Module      : SDL.Raw.Image
Copyright   : (c) 2015 Siniša Biđin
License     : MIT
Maintainer  : sinisa@bidin.eu
Stability   : experimental

Raw bindings to the @SDL2_image@ library. No error-handling is done here. For
more information about specific function behaviour, see the @SDL2_image@
documentation.

-}

{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}

module SDL.Raw.Image
  (
  -- * Loading images
    Free
  , load
  , load_RW
  , Format
  , loadTyped_RW
  , loadCUR_RW
  , loadICO_RW
  , loadBMP_RW
  , loadPNM_RW
  , loadXPM_RW
  , loadXCF_RW
  , loadPCX_RW
  , loadGIF_RW
  , loadJPG_RW
  , loadTIF_RW
  , loadPNG_RW
  , loadTGA_RW
  , loadLBM_RW
  , loadXV_RW
  , loadWEBP_RW

  -- * Testing for formats
  , isCUR
  , isICO
  , isBMP
  , isPNM
  , isXPM
  , isXCF
  , isPCX
  , isGIF
  , isJPG
  , isTIF
  , isPNG
  , isLBM
  , isXV
  , isWEBP

  -- * Other
  , InitFlags
  , pattern IMG_INIT_JPG
  , pattern IMG_INIT_PNG
  , pattern IMG_INIT_TIF
  , pattern IMG_INIT_WEBP
  , init
  , getVersion
  , quit
  ) where

#include "SDL_image.h"

import Foreign.C.String       (CString)
import Foreign.C.Types        (CInt(..))
import Foreign.Ptr            (Ptr)
import Prelude         hiding (init)
import SDL.Raw.Types          (Version, Surface, RWops)
import SDL.Raw.Helper         (liftF)

liftF "getVersion" "IMG_Linked_Version"
  [t|IO (Ptr Version)|]

type InitFlags = CInt

pattern IMG_INIT_JPG  = #{const IMG_INIT_JPG}
pattern IMG_INIT_PNG  = #{const IMG_INIT_PNG}
pattern IMG_INIT_TIF  = #{const IMG_INIT_TIF}
pattern IMG_INIT_WEBP = #{const IMG_INIT_WEBP}

liftF "init" "IMG_Init"
  [t|InitFlags -> IO InitFlags|]

liftF "quit" "IMG_Quit"
  [t|IO ()|]

liftF "load" "IMG_Load"
  [t|CString -> IO (Ptr Surface)|]

-- | Should the 'Ptr' 'RWops' be freed after an operation? 1 for yes, 0 for no.
type Free = CInt

liftF "load_RW" "IMG_Load_RW"
  [t|Ptr RWops -> Free -> IO (Ptr Surface)|]

-- | A case-insensitive desired format, e.g. @\"jpg\"@ or @\"PNG\"@.
type Format = CString

liftF "loadTyped_RW" "IMG_LoadTyped_RW"
  [t|Ptr RWops -> Free -> Format -> IO (Ptr Surface)|]

liftF "loadCUR_RW"  "IMG_LoadCUR_RW"  [t|Ptr RWops -> IO (Ptr Surface)|]
liftF "loadICO_RW"  "IMG_LoadICO_RW"  [t|Ptr RWops -> IO (Ptr Surface)|]
liftF "loadBMP_RW"  "IMG_LoadBMP_RW"  [t|Ptr RWops -> IO (Ptr Surface)|]
liftF "loadPNM_RW"  "IMG_LoadPNM_RW"  [t|Ptr RWops -> IO (Ptr Surface)|]
liftF "loadXPM_RW"  "IMG_LoadXPM_RW"  [t|Ptr RWops -> IO (Ptr Surface)|]
liftF "loadXCF_RW"  "IMG_LoadXCF_RW"  [t|Ptr RWops -> IO (Ptr Surface)|]
liftF "loadPCX_RW"  "IMG_LoadPCX_RW"  [t|Ptr RWops -> IO (Ptr Surface)|]
liftF "loadGIF_RW"  "IMG_LoadGIF_RW"  [t|Ptr RWops -> IO (Ptr Surface)|]
liftF "loadJPG_RW"  "IMG_LoadJPG_RW"  [t|Ptr RWops -> IO (Ptr Surface)|]
liftF "loadTIF_RW"  "IMG_LoadTIF_RW"  [t|Ptr RWops -> IO (Ptr Surface)|]
liftF "loadPNG_RW"  "IMG_LoadPNG_RW"  [t|Ptr RWops -> IO (Ptr Surface)|]
liftF "loadTGA_RW"  "IMG_LoadTGA_RW"  [t|Ptr RWops -> IO (Ptr Surface)|]
liftF "loadLBM_RW"  "IMG_LoadLBM_RW"  [t|Ptr RWops -> IO (Ptr Surface)|]
liftF "loadXV_RW"   "IMG_LoadXV_RW"   [t|Ptr RWops -> IO (Ptr Surface)|]
liftF "loadWEBP_RW" "IMG_LoadWEBP_RW" [t|Ptr RWops -> IO (Ptr Surface)|]

liftF "isCUR"  "IMG_isCUR"  [t|Ptr RWops -> IO CInt|]
liftF "isICO"  "IMG_isICO"  [t|Ptr RWops -> IO CInt|]
liftF "isBMP"  "IMG_isBMP"  [t|Ptr RWops -> IO CInt|]
liftF "isPNM"  "IMG_isPNM"  [t|Ptr RWops -> IO CInt|]
liftF "isXPM"  "IMG_isXPM"  [t|Ptr RWops -> IO CInt|]
liftF "isXCF"  "IMG_isXCF"  [t|Ptr RWops -> IO CInt|]
liftF "isPCX"  "IMG_isPCX"  [t|Ptr RWops -> IO CInt|]
liftF "isGIF"  "IMG_isGIF"  [t|Ptr RWops -> IO CInt|]
liftF "isJPG"  "IMG_isJPG"  [t|Ptr RWops -> IO CInt|]
liftF "isTIF"  "IMG_isTIF"  [t|Ptr RWops -> IO CInt|]
liftF "isPNG"  "IMG_isPNG"  [t|Ptr RWops -> IO CInt|]
liftF "isLBM"  "IMG_isLBM"  [t|Ptr RWops -> IO CInt|]
liftF "isXV"   "IMG_isXV"   [t|Ptr RWops -> IO CInt|]
liftF "isWEBP" "IMG_isWEBP" [t|Ptr RWops -> IO CInt|]
