{-# LANGUAGE FunctionalDependencies #-}

module SDL.Image.Internal.Numbered where

class (Integral b) => FromNumber a b | a -> b where
  fromNumber :: b -> a

class (Integral b) => ToNumber a b | a -> b where
  toNumber :: a -> b
