module Main where

import System.Environment (getArgs)
import Text.Read (readMaybe)

import Fractals.Mandelbrot (generateMandelbrot)

defaults :: (Int, Int, Int, Double, (Double, Double), (Double, Double), FilePath)
defaults = (1920, 1080, 1024, 2.0, (-2.0, 1.0), (-1.5, 1.5), "mandelbrot.png")

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> do
      let (width, height, maxIter, radius, xRange, yRange, outFile) = defaults
      putStrLn $ "Generating Mandelbrot image: " ++ outFile
      putStrLn "Using default arguments"
      generateMandelbrot width height maxIter radius xRange yRange outFile

    [widthStr, heightStr, maxIterStr, radiusStr, xMinStr, xMaxStr, yMinStr, yMaxStr, outFile] -> do
      case ( readMaybe widthStr
           , readMaybe heightStr
           , readMaybe maxIterStr
           , readMaybe radiusStr
           , readMaybe xMinStr
           , readMaybe xMaxStr
           , readMaybe yMinStr
           , readMaybe yMaxStr
           ) of
        (Just width, Just height, Just maxIter, Just radius, Just xMin, Just xMax, Just yMin, Just yMax) ->
          generateMandelbrot width height maxIter radius (xMin, xMax) (yMin, yMax) outFile
        _ -> putStrLn "Invalid arguments"

    _ -> putStrLn "Usage: hs-fractals <width> <height> <maxIter> <radius> <xMin> <xMax> <yMin> <yMax> <outFile>"
