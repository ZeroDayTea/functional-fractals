module Fractals.Mandelbrot (generateMandelbrot) where

import System.IO (withFile, IOMode(WriteMode), hPutStr)

type Color = (Int, Int, Int)

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

  withFile outFile WriteMode $ \handle -> do
    hPutStr handle $ "P3\n" ++ show width ++ " " ++ show height ++ "\n255\n"
    mapM_ (writeRow handle dx dy) [0 .. height-1]

  where
    writeRow handle dx dy row = do
      let y0 = yMin + fromIntegral row * dy
      let rowPixels = [ pixelColor col y0 dx | col <- [0 .. width-1] ]
      mapM_ (writePixel handle) rowPixels

    pixelColor col y0 dx =
      let x0 = xMin + fromIntegral col * dx
          iterCount = mandelbrotIters radius maxIter x0 y0
          (r, g, b) = iterationToColor maxIter iterCount
      in (r, g, b)

    writePixel handle (r, g, b) =
      hPutStr handle (show r ++ " " ++ show g ++ " " ++ show b ++ "\t")
