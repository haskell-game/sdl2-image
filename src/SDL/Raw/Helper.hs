{-|

Module      : SDL.Raw.Helper
Copyright   : (c) 2015 Siniša Biđin
License     : MIT
Maintainer  : sinisa@bidin.eu
Stability   : experimental

Exposes a way to automatically generate a foreign import alongside its lifted,
inlined MonadIO variant. Use this to simplify the package's SDL.Raw.* modules.

-}

{-# LANGUAGE BangPatterns    #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE TemplateHaskell #-}

module SDL.Raw.Helper (liftF) where

import Control.Monad           (replicateM)
import Control.Monad.IO.Class  (MonadIO, liftIO)
import Language.Haskell.TH

-- | Given a name @fname@, a name of a C function @cname@ and the desired
-- Haskell type @ftype@, this function generates:
--
-- * A foreign import of @cname@, named as @fname'@.
-- * An always-inline MonadIO version of @fname'@, named @fname@.
liftF :: String -> String -> Q Type -> Q [Dec]
liftF fname cname ftype = do
  let f' = mkName $ fname ++ "'" -- Direct binding.
  let f  = mkName fname          -- Lifted.
  t' <- ftype                    -- Type of direct binding.

  -- The generated function accepts n arguments.
  args <- replicateM (countArgs t') $ newName "x"

  -- If the function has no arguments, then we just liftIO it directly.
  -- However, this fails to typecheck without an explicit type signature.
  -- Therefore, we include one. TODO: Can we get rid of this?
  sigd <- case args of
            [] -> ((:[]) . SigD f) `fmap` liftType t'
            _  -> return []

  return $ concat
    [
      [ ForeignD $ ImportF CCall Safe cname f' t'
      , PragmaD $ InlineP f Inline FunLike AllPhases
      ]
    , sigd
    , [ FunD f
        [ Clause
            (map VarP args)
            (NormalB $ 'liftIO `applyTo` [f' `applyTo` map VarE args])
            []
        ]
      ]
    ]

-- | How many arguments does a function of a given type take?
countArgs :: Type -> Int
countArgs = count 0
  where
    count !n = \case
      (AppT (AppT ArrowT _) t) -> count (n+1) t
      (ForallT _ _ t) -> count n t
      (SigT t _)      -> count n t
      _               -> n

-- | An expression where f is applied to n arguments.
applyTo :: Name -> [Exp] -> Exp
applyTo f [] = VarE f
applyTo f es = loop (tail es) . AppE (VarE f) $ head es
  where
    loop as e = foldl AppE e as

-- | Fuzzily speaking, converts a given IO type into a MonadIO m one.
liftType :: Type -> Q Type
liftType = \case
  AppT _ t -> do
    m <- newName "m"
    return $
      ForallT
        [PlainTV m]
        [AppT (ConT ''MonadIO) $ VarT m]
        (AppT (VarT m) t)
  t -> return t
