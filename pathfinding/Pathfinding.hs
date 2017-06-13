{-
Tools for importing / exporting images to pathfinding maps. Uses the
friday-devil package.
-}

{-# LANGUAGE ScopedTypeVariables #-}

import System.Environment (getArgs)
import Criterion.Main
import System.Directory (doesFileExist, removeFile)
import Control.Monad (when)
import qualified Data.Vector as Vector
import qualified Data.Vector.Storable as Storable
import qualified Data.Vector.Unboxed as Unboxed
import qualified Data.Map as Map
import Data.List
import Debug.Trace
import Options.Applicative
import Data.Semigroup ((<>))

import qualified Vision.Image as Image
import Vision.Image.Storage.DevIL (Autodetect (..), PNG (..), load, save)
import Vision.Primitive (ix2)
import Vision.Primitive.Shape

import Grid
import qualified JPS as JPS
import qualified Astar as Astar


main::IO ()
main = 
  do

    -- TODO : make an actual command-line interface instead of hardcoding :)
    
  -- [input, output] <- getArgs
    {-
  input  <- return "../maps/AR0011SRBW.jpg"
  start  <- return (Coord 414 418)
  finish <- return (Coord 253 130)
-}

    {-
  input  <- return "../maps/AR0017SRBW.jpg"
  start  <- return (Coord 70 54)
  finish <- return (Coord 43 42)
-}

  input  <- return "../maps/CalderaBW.jpg"
  finish <- return (Coord 140 511)
  start  <- return (Coord 572 56)
{-    
-}

  
  output <- return "./testout.png"
  
  --finish <- return (Coord 420 393)
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
      
      let
        grid   = imageToGrid rgb
        dim    = dims grid
        start  = c2i dim startc
        finish = c2i dim finishc
        
      {-
      defaultMain [
        bench "searchAstar" $ whnf (\x -> Astar.findPathNormal x start finish) grid
        ]
      -}
      (path, visited) <- return (Astar.findPathNormal grid start finish)

      {-
      defaultMain [
        bench "searchJPS" $ whnf (\x -> JPS.findPathJPS x start finish) grid
        ]
      
      (path, visited) <- return (JPS.findPathJPS grid start finish)
      -}
      
      grid' <- return
        $ (markStartFinish start finish)
        $ (markPath path)
        $ (markVisited visited)
        $ grid
      
      case path of
        [] -> do
          putStrLn("No path found!")
        otherwise -> do
          putStrLn("Found a path!")
          
      image <- return (gridToImage grid')
    
      outExists <- doesFileExist pathOut
      when outExists (removeFile pathOut)
      
      mErr <- save Autodetect pathOut image
      case mErr of
        Nothing -> return ()
        Just err -> do
          putStrLn $ "ERROR - could not save image to path: " ++ pathOut
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
                     2 -> (Image.RGBPixel 255   0 255)   -- Start
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

markVisited :: Map.Map Int Int -> Grid -> Grid
markVisited visited (Grid dims sqs) =
  let
   sqs' = Unboxed.imap (\ i x -> if Map.member i visited then 4 else x) sqs
  in
    (Grid dims sqs')

markPath ::  [Int] -> Grid ->  Grid
markPath path (Grid dims sqs) =
  let
    zipped = [(i, 1) | i <- path]
    pathset = Map.fromList zipped
    sqs' = Unboxed.imap (\ i x -> if Map.member i pathset then 5 else x) sqs
  in
    (Grid dims sqs')

markStartFinish :: Int -> Int -> Grid -> Grid
markStartFinish start finish (Grid dims sqs) =
  let
   sqs' = Unboxed.imap (markSF start finish) sqs
  in
    (Grid dims sqs')

markSF :: Int -> Int -> Int -> Square -> Square
markSF start finish i x | i == start  = 2
                        | i == finish = 3
                        | otherwise   = x

