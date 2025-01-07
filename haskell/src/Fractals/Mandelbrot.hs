module Fractals.Mandelbrot (generateMandelbrot) where

import System.IO (withFile, IOMode(WriteMode), hPutStr)
import Codec.Picture
import Data.Word (Word8)

type Color = (Word8, Word8, Word8)

colorMapping :: [Color]
colorMapping =
    [ (66, 30, 15)
    , (25, 7, 26)
    , (9, 1, 47)
    , (4, 4, 73)
    , (0, 7, 100)
    , (12, 44, 138)
    , (24, 82, 177)
    , (57, 125, 209)
    , (134, 181, 229)
    , (211, 236, 248)
    , (241, 233, 191)
    , (248, 201, 95)
    , (255, 170, 0)
    , (204, 128, 0)
    , (153, 87, 0)
    , (106, 52, 3)
    ]

mandelbrotIters :: Double -> Int -> Double -> Double -> Int
mandelbrotIters radius maxIter cx cy = go 0 (0, 0)
  where
    go :: Int -> (Double, Double) -> Int
    go iter (x, y)
      | iter >= maxIter = maxIter
      | x*x + y*y > radius^2 = iter
      | otherwise =
        let x' = x*x - y*y + cx
            y' = 2*x*y + cy
        in go (iter+1) (x', y')

iterationToColor :: Int -> Int -> Color
iterationToColor maxIter iter
  | iter == maxIter = (0, 0, 0)
  | iter > 0 = colorMapping !! (iter `mod` 16)
  | otherwise = (0, 0, 0)

generateMandelbrot :: Int -> Int -> Int -> Double -> (Double, Double) -> (Double, Double) -> FilePath -> IO()
generateMandelbrot width height maxIter radius (xMin, xMax) (yMin, yMax) outFile = do
  let dx = (xMax - xMin) / fromIntegral width
      dy = (yMax - yMin) / fromIntegral height

      pixelGen x y =
        let cx = xMin + fromIntegral x * dx
            cy = yMin + fromIntegral y * dy
            iter = mandelbrotIters radius maxIter cx cy
            (r, g, b) = iterationToColor maxIter iter
        in PixelRGB8 r g b

      image = generateImage pixelGen width height

  writePng outFile image
