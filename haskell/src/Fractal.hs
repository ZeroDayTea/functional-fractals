module Fractals.Mandelbrot (generateMandelbrot) where

import System.IO (withFile, IOMode(WriteMode), hPutStr)

type Color = (Int, Int, Int)

mandelbrotIters :: Double -> Int -> Double -> Double -> Int
mandelbrotIters radius maxIter cx cy = go 0 0 0
  where
    go :: Int -> (Double, Double) -> Int
    go iter (x, y)
      | iter >= maxIter = maxIter
      | x*x + y*y > radius^2 = iter
      | otherwise =
        let x' = x*x - y*y + cx
            y' = 2*x*y + cy
        in go (iter+1) (x', y')

    go iter xy = go iter xy

iterationToColor :: Int -> Int -> Color
iterationToColor maxIter iter =
  if iter == maxIter
     then (0, 0, 0)
     else
       let c = round (255 * fromIntegral iter / fromIntegral maxIter)
       in (c, 0, 255 - c)

generateMandelbrot :: Int -> Int -> Int -> Double -> (Double, Double) -> (Double, Double) -> FilePath -> IO()
generateMandelbrot width height maxIter radius (xMin, xMax) (yMin, yMax) outFile = do
