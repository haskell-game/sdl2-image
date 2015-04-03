{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Prelude hiding (putStrLn)
import Data.Text (Text)
import Data.Text.IO (putStrLn)
import System.Environment (getArgs)
import Control.Concurrent (threadDelay)

import qualified SDL
import qualified SDL.Image as IMG

-- A sequence of example actions to be perfomed and displayed.
examples :: [(Text, SDL.Window -> FilePath -> IO ())]
examples = [

  ("Loading as surface, blitting",
    \window path -> do
      image <- IMG.load path
      screen <- SDL.getWindowSurface window
      SDL.blitSurface image Nothing screen Nothing
      SDL.updateWindowSurface window
      SDL.freeSurface image),

  ("Loading as texture, rendering",
    \window path -> do
       r <- SDL.createRenderer window (-1) SDL.defaultRenderer
       texture <- IMG.loadTexture r path
       SDL.renderClear r
       SDL.renderCopy r texture Nothing Nothing
       SDL.renderPresent r)]

main :: IO ()
main = do
  SDL.initialize [SDL.InitVideo]

  getArgs >>= \case
    [] -> putStrLn "Usage: cabal run path/to/image.(png|jpg|...)"
    (path:_) ->
      flip mapM_ examples $ \(name, action) -> do
        putStrLn name
        window <- SDL.createWindow name SDL.defaultWindow
        SDL.showWindow window
        action window path
        threadDelay 1000000
        SDL.destroyWindow window

  SDL.quit
