module Main where

import Fractals.Mandelbrot (generateMandelbrot)

main :: IO ()
main = do
  let width = 800
      height = 600
      radius = 2.0
      maxIter = 512
      xRange = (-2.0, 1.0)
      yRange = (-1.5, 1.5)
      outFile = "mandelbrot.ppm"

  putStrLn $ "Generating Mandelbrot image: " ++ outFile
  generateMandelbrot width height maxIter radius xRange yRange outFile
