module Main where

-- import qualified SDL
import qualified SDL.Image as IMG

main :: IO ()
main = do
  -- SDL.initialize [SDL.InitEverything]
  -- print =<< SDL.version
  print =<< IMG.version
  IMG.initialize [IMG.InitPNG]
  IMG.quit
