{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Environment (getArgs)
import Control.Concurrent (threadDelay)

import qualified SDL
import qualified SDL.Image as IMG

main :: IO ()
main = do
  SDL.initialize [SDL.InitVideo]
  window <- SDL.createWindow "SDL_image test" SDL.defaultWindow

  args <- getArgs
  case args of
    [] -> putStrLn "Usage: cabal run path/to/image.(png|jpg|...)"
    (path:_) -> do
      putStrLn $ "Loading and displaying " ++ path
      IMG.initialize [IMG.InitPNG, IMG.InitJPG]
      image <- IMG.load path
      IMG.quit

      screen <- SDL.getWindowSurface window
      SDL.blitSurface image Nothing screen Nothing
      SDL.updateWindowSurface window
      SDL.showWindow window
      SDL.freeSurface image
      threadDelay $ 1000000*2

  SDL.destroyWindow window
  SDL.quit
