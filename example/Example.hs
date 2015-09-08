{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Concurrent (threadDelay)
import Control.Monad      (forM_)
import Data.Text          (Text)
import Data.Text.IO       (putStrLn)
import Prelude     hiding (putStrLn)
import System.Environment (getArgs)
import System.Exit        (exitFailure)

import qualified SDL
import qualified SDL.Image

-- A sequence of example actions to be perfomed and displayed.
examples :: [(Text, SDL.Window -> FilePath -> IO ())]
examples = [

  ("Loading as surface, blitting",
    \window path -> do
      image <- SDL.Image.load path
      screen <- SDL.getWindowSurface window
      SDL.surfaceBlit image Nothing screen Nothing
      SDL.updateWindowSurface window
      SDL.freeSurface image),

  ("Loading as texture, rendering",
    \window path -> do
       r <- SDL.createRenderer window (-1) SDL.defaultRenderer
       texture <- SDL.Image.loadTexture r path
       SDL.clear r
       SDL.copy r texture Nothing Nothing
       SDL.present r
       SDL.destroyTexture texture)]

main :: IO ()
main = do

  SDL.initialize [SDL.InitVideo]

  getArgs >>= \case

    [] -> do
      putStrLn "Usage: cabal run path/to/image.(png|jpg|...)"
      exitFailure

    (path:_) ->
      -- Run each of the examples within a newly-created window.
      forM_ examples $ \(name, action) -> do
        putStrLn name
        window <- SDL.createWindow name SDL.defaultWindow
        SDL.showWindow window
        action window path
        threadDelay 1000000
        SDL.destroyWindow window

  SDL.quit
