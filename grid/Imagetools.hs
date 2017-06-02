{-
Tools for importing / exporting images to pathfinding maps. Uses the
friday-devil package.
-}

{-# LANGUAGE ScopedTypeVariables #-}

import System.Environment (getArgs)

import Vision.Image
import Vision.Image.Storage.DevIL (Autodetect (..), load, save)
import Vision.Primitive (ix2)

import Grid


main::IO ()
main = do
  -- [input, output] <- getArgs

  input <- return "../maps/AR0011SR.jpg"
  putStrLn ("Loading: " ++ input)
  
{-
-}
  io <- load Autodetect input

  case io of
    Left err -> do
      putStrLn "Unable to load image"
      print err 

    Right (rgb :: RGB) -> do
      putStrLn "It worked!"

-- Do a pathfinding operation on the image at pathIn; save the
-- resulting image to pathOut.
pathfindImage :: FilePath -> FilePath ->  IO ()
pathfindImage pathIn pathOut = do

  io <- load Autodetect pathIn

  case io of
    Left err -> do
      putStrLn ("Unable to load image at path: " ++ pathIn)
      print err

    Right (rgb :: RGB) -> do
      grid <- return (emptyGrid (GridDims 0 0))
      putStrLn ("Loaded: " ++ pathIn)
      putStrLn ("Loaded: " ++ pathIn)

  
