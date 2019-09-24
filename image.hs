import System.Environment
import Linear

-- main = putStrLn $ makePicture 200 100
main = do
  n <- getProgName
  writeFile (n++".ppm") $ makePicture 500 250

makePicture :: Int -> Int -> String
makePicture w h = header ++ body
  where
    header = "P3\n"
      ++ show w ++ " "
      ++ show h ++ "# width height (pixels)\n"
      ++ "255 # max intensity\n\n"
    body = concat $ do
      j <- map fromIntegral [h-1,h-2..0]
      i <- map fromIntegral [0..w-1]
      let r = floor $ 255.99* (i/(fromIntegral w))
      let g = floor $ 255.99* (j/(fromIntegral h))
      let b = floor $ 255.99* (0.7)
      return $ makePixel r g b

makePixel :: Int -> Int -> Int -> String
makePixel r g b = show r ++ " " ++ show g ++ " " ++ show b ++ "\n"
