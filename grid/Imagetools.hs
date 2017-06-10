{-
Tools for importing / exporting images to pathfinding maps. Uses the
friday-devil package.
-}

{-# LANGUAGE ScopedTypeVariables #-}

import System.Environment (getArgs)
import System.TimeIt
import Criterion.Main
import System.Directory (doesFileExist, removeFile)
import Control.Monad (when)
import qualified Data.Vector as Vector
import qualified Data.Vector.Storable as Storable
import qualified Data.Vector.Unboxed as Unboxed
import qualified Data.HashMap.Strict as HashMap
import Data.List

import Control.DeepSeq(force)
import Control.Exception(evaluate)

import qualified Vision.Image as Image
import Vision.Image.Storage.DevIL (Autodetect (..), load, save)
import Vision.Primitive (ix2)
import Vision.Primitive.Shape

import Grid
import qualified Astar as Astar

main::IO ()
main = 
  do
  -- [input, output] <- getArgs

  input  <- return "../maps/AR0017SRBWhighres.jpg"
  output <- return "./testout.jpg"
  start  <- return (Coord 584 438)
  finish <- return (Coord 309 175)
  --finish <- return (Coord 420 393)
  putStrLn ("Loading: " ++ input)
  putStrLn ("Start: " ++ (show start))
  putStrLn ("finish: " ++ (show finish))
  pathfindImage input output start finish
  
-- Do a pathfinding operation on the image at pathIn; save the
-- resulting image to pathOut.
pathfindImage :: FilePath -> FilePath -> Coord -> Coord -> IO ()
pathfindImage pathIn pathOut startc finishc = do
  
  io <- load Autodetect pathIn

  case io of
    Left err -> do
      putStrLn ("Unable to load image at path: " ++ pathIn)
      print err

    Right (rgb :: Image.RGB) -> do
      
      putStrLn ("Loaded: " ++ pathIn)
      
      grid          <- return (imageToGrid rgb)
      dim           <- return (dims grid)
      start         <- return (c2i dim startc)
      finish        <- return (c2i dim finishc)
      putStrLn("starting...")

      defaultMain [
        bench "search" $ whnf (\x -> Astar.findPathNormal x start finish) grid
        ]
      (path, grid') <- return (Astar.findPathNormal grid start finish)
      
      case path of
        Just ps -> do
          putStrLn("Found a path!")
        Nothing -> do
          putStrLn("No path found!")
          
      image <- return (gridToImage grid')
    
      putStrLn ("Attempting to save to: " ++ pathIn)
      outExists <- doesFileExist pathOut
      when outExists (removeFile pathOut)
      
      mErr <- save Autodetect pathOut image
      case mErr of
        Nothing ->
          putStrLn "Success!"
        Just err -> do
          putStrLn "ERROR - could not save image"
          print err
  
-- Convert coordinates to / from the Shape format used by Vision (we only need 2d coordinates :) )
dim2ToCoord (((d0 :. d1) :: DIM1) :. d2) = (Coord d1 d2)
  
-- Convert a coordinate back to a Shape
coordToDim2 :: Coord -> DIM2
coordToDim2 (Coord x y) = ix2 x y

-- Convert a colored pixel into the corresponding square type
colorToSquare :: Image.RGBPixel -> Square
colorToSquare (Image.RGBPixel r g b) = if (r < 10) && (b < 10) && (g < 10) then 0 else 1

-- Convert a square to a nice colorful pixel
squareToColor :: Square -> Image.RGBPixel
squareToColor sq = case sq of
                     0 -> (Image.RGBPixel 0   0   0)   -- Blocked
                     1 -> (Image.RGBPixel 255 255 255) -- Open 
                     2 -> (Image.RGBPixel 0   255 0)   -- Start
                     3 -> (Image.RGBPixel 0   0   255) -- Finish
                     4 -> (Image.RGBPixel 244 158 66)  -- Visited
                     5 -> (Image.RGBPixel 66 212 244)  -- On Path
                      
-- Convert an RGB image into a Grid 
imageToGrid :: Image.RGB -> Grid
imageToGrid (Image.Manifest manifestSize manifestVector) =
  let
    image             = Storable.toList manifestVector
    (Coord x y)       = dim2ToCoord manifestSize
    dims              = (GridDims x y)
    squares           = map colorToSquare image
    squaresWithIndices = zip [0..] squares
    squaresWithCoords = [(dim2ToCoord (fromLinearIndex manifestSize index), sq)
                        | (index, sq) <- squaresWithIndices]
    grid              = Unboxed.fromList squares
    in Grid dims grid 

-- Convert a grid to a corresponding image
gridToImage :: Grid -> Image.RGB
gridToImage (Grid (GridDims dimx dimy) grid) =
  let
    gridshape  = coordToDim2 (Coord dimx dimy)
    pixels     = map squareToColor (Unboxed.toList grid)
  in Image.Manifest gridshape (Storable.fromList pixels)
    
