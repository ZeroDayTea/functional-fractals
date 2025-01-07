module Main where

import Fractals.Mandelbrot (generateMandelbrot)

main :: IO ()
main = do
  let width = 1920
      height = 1080
      radius = 2.0
      maxIter = 1024
      xRange = (-2.0, 1.0)
      yRange = (-1.5, 1.5)
      outFile = "mandelbrot.png"

  putStrLn $ "Generating Mandelbrot image: " ++ outFile
  generateMandelbrot width height maxIter radius xRange yRange outFile
