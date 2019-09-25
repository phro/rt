import System.Environment
import Linear
import Control.Lens

type Vec   = V3 Double
type Color = V3 Int

data Ray = Ray {
  orig :: Vec,
  dir  :: Vec
               } deriving (Show)

pointAtParam :: Ray -> Double -> Vec
pointAtParam r t = orig r + t*^(dir r)

main = do
  n <- getProgName
  writeFile (n++".ppm") $ makePicture 200 100

-- Helper functions
makePixel :: Color -> String
makePixel (V3 r g b) = show r ++ " " ++ show g ++ " " ++ show b ++ "\n"

rescale :: Double -> Int
rescale = floor . (255.99*) 

fromVec :: Vec -> Color
fromVec = fmap rescale


-- Practice functions

color :: Ray -> Color
color r = fromVec $ lerp t v w
  where
    t = 0.5*(1-(signorm (dir r) ^. _y))
    v = V3 1 1 1
    w = V3 0.5 0.7 1

makePicture :: Int -> Int -> String
makePicture w h = header ++ body
  where
    header = "P3\n"
      ++ show w ++ " "
      ++ show h ++ "# width height (pixels)\n"
      ++ "255 # max intensity\n\n"
    lowerLeftCorner = V3 (-2) (-1) (-1)
    horizontal = V3 4 0 0
    vertical = V3 0 2 0
    body = concat $ do
      j <- map fromIntegral [h-1,h-2..0]
      i <- map fromIntegral [0..w-1]
      let u = i/(fromIntegral w)
      let v = j/(fromIntegral h)
      let r = Ray {orig=zero, dir= lowerLeftCorner + u*^horizontal + v*^vertical}
      let c = color r
      return $ makePixel c
