module Main where

import Fractals.Mandelbrot ()

main :: IO ()
main = do
  let outFile = "mandelbrot.ppm"
  putStrLn $ "Generating Mandelbrot image: " ++ outFile
