import System.Environment
import Linear
import Control.Lens

type Vec   = V3 Double
type Color = V3 Int

data Ray = Ray
  {orig :: Vec
  ,dir  :: Vec
  } deriving (Show)

infinity :: Double
infinity = (read "Infinity")::Double

pointAtParam :: Ray -> Double -> Vec
pointAtParam r t = orig r + t*^(dir r)

-- Helper functions
makePixel :: Color -> String
makePixel (V3 r g b) = show r ++ " " ++ show g ++ " " ++ show b ++ "\n"

rescale :: Double -> Int
rescale = floor . (255.99*)

fromVec :: Vec -> Color
fromVec = fmap rescale

{-
  We want an object to be hittable.
 -}

data HitRecord = Miss
               | HitRecord
                 {time :: Double
                 ,pt :: Vec
                 ,normal :: Vec
                 } deriving (Eq)

instance Ord HitRecord where
  Miss            `compare` Miss            = EQ
  Miss            `compare` HitRecord _ _ _ = GT
  HitRecord _ _ _ `compare` Miss            = LT
  h1              `compare` h2              = (time h1) `compare` (time h2)

class Object a where
  hit :: Ray
      -> Double    -- tmin
      -> Double    -- tmax
      -> a
      -> HitRecord

instance (Object a) => Object [a] where
  hit r tmin tmax o =
    -- TODO: tmax below should depend on how many objects have been hit so far.
    let hs = map (hit r tmin tmax) o
     in case hs of
          [] -> Miss
          _  -> foldr1 min hs

data Sphere = Sphere
  {centre :: Vec
  ,radius :: Double
  } deriving (Show, Eq)

instance Object Sphere where
  hit r tmin tmax s =
    let
      k = orig r - (centre s)
      a = dot (dir r) (dir r)
      b = dot k (dir r)
      c = dot k k-(radius s)^2
      d = b^2 - a*c
    in
      if (d < 0) then Miss
       else
         let d' = sqrt d
             x1 = (-b-d')/a
         in
           if tmin < x1 && x1 < tmax
           then let p = pointAtParam r x1
                in HitRecord x1 p (signorm (p - (centre s)))
           else
            let x2 = (-b+d')/a in
              if tmin < x2 && x2 < tmax
              then let p = pointAtParam r x1
                    in HitRecord x2 p (signorm (p - (centre s)))
              else Miss

-- Practice functions

color :: (Object a) => Ray -> a -> Color
color r o =
  case hit r 0 infinity o of
    Miss -> fromVec $ lerp s v w
    h    -> fromVec $ 0.5*^ (normal h + V3 1 1 1)
  where -- background gradient
    s = 0.5*(1-((dir r) ^. _y))
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
    world =
      [Sphere (V3 0 0 (-1)) 0.5
      , Sphere (V3 0 (-100.5) (-1)) 100
      ]
    body = concat $ do
      j <- map fromIntegral [h-1,h-2..0]
      i <- map fromIntegral [0..w-1]
      let u = i/(fromIntegral w)
      let v = j/(fromIntegral h)
      let r = Ray {orig=zero, dir= lowerLeftCorner + u*^horizontal + v*^vertical}
      let c = color r world
      return $ makePixel c

main = do
  n <- getProgName
  writeFile (n++".ppm") $ makePicture 200 100
