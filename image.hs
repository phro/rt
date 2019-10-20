import System.Environment
import Linear
import Control.Lens

type Color = V3 Double

data Ray = Ray
  {orig :: V3 Double
  ,dir  :: V3 Double
  } deriving (Show)

infinity :: Double
infinity = (read "Infinity")::Double

pointAtParam :: Ray -> Double -> V3 Double
pointAtParam r t = orig r + t*^(dir r)

-- Helper functions
makePixel :: Color -> String
makePixel v = show r ++ " " ++ show g ++ " " ++ show b ++ "\n"
  where (V3 r g b) = fromVec v

rescale :: Double -> Int
rescale = floor . (255.99*)

fromVec :: V3 Double -> V3 Int
fromVec = fmap rescale

data HitRecord = Miss
               | HitRecord
                 {time :: Double
                 ,pt :: V3 Double
                 ,normal :: V3 Double
                 } deriving (Eq)

instance Ord HitRecord where
  Miss            `compare` Miss            = EQ
  Miss            `compare` HitRecord _ _ _ = GT
  HitRecord _ _ _ `compare` Miss            = LT
  h1              `compare` h2              = (time h1) `compare` (time h2)

class EMO a where -- ElectroMagnetic Object
  hit :: Ray
      -> Double    -- tmin
      -> Double    -- tmax
      -> a
      -> HitRecord

instance (EMO a) => EMO [a] where
  hit r tmin tmax os = foldr (\o h->
    case h of
      Miss -> hit r tmin tmax o
      _    -> min h $ hit r tmin (time h) o
                             ) Miss os

data Sphere = Sphere
  {centre :: V3 Double
  ,radius :: Double
  } deriving (Show, Eq)

instance EMO Sphere where
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

data Camera = Camera
  {lowerLeftCorner :: V3 Double
  ,horizontal :: V3 Double
  ,vertical :: V3 Double
  ,origin :: V3 Double
  ,width :: !Int
  ,height :: !Int
  }

getRay :: Camera -> V2 Double -> Ray
getRay c (V2 u v) = Ray
  {orig = origin c
  ,dir = lowerLeftCorner c + u*^horizontal c + v*^vertical c - origin c
  }

 -- Extraneous function
getRayInt :: Camera -> V2 Int -> Ray
getRayInt c (V2 i j) = Ray
  {orig = origin c
  ,dir = lowerLeftCorner c
       + (fromIntegral i/(fromIntegral $ width c))*^horizontal c
       + (fromIntegral j/(fromIntegral $ height c))*^vertical c - origin c
  }

-- Practice functions

color :: (EMO a) => a -> Ray -> Color
color o r =
  case hit r 0 infinity o of
    Miss -> lerp s v w
    h    -> 0.5*^ (normal h + V3 1 1 1)
  where -- background gradient
    s = 0.5*(1-((dir r) ^. _y))
    v = V3 1 1 1
    w = V3 0.5 0.7 1

perturb :: (Camera -> V2 Double -> Color) -- raycaster
        -> Camera
        -> V2 Double           -- pixel screen coordinate
        -> [V2 Double]         -- list of pixel perturbations
        -> Color
perturb f ca co ps
  = recip (fromIntegral $ length ps) *^ (sum $ fmap (f ca . (+co).
    (\(V2 u v )->
      V2 (u/(fromIntegral $ width ca)) (v/(fromIntegral $ height ca)))) ps)

makePicture :: Int -> Int -> Int -> String
makePicture w h aa = header ++ body
  where
    header = "P3\n"
      ++ show w ++ " "
      ++ show h ++ "# width height (pixels)\n"
      ++ "255 # max intensity\n\n"
    cam = Camera
      {lowerLeftCorner = V3 (-2) (-1) (-1)
      ,horizontal = V3 4 0 0
      ,vertical = V3 0 2 0
      ,origin = zero
      ,width = w
      ,height = h
      }
    world =
      [Sphere (V3 0 0 (-1)) 0.5
      ,Sphere (V3 0 (-100.5) (-1)) 100
      ]
    body = concat $ do
      j <- map fromIntegral [h-1,h-2..0]
      i <- map fromIntegral [0..w-1]
      let n = fromIntegral aa
      let jigs = [V2 (k/n)(l/n) | k<-[0..n-1],l<-[0..n-1]]
      let u = fromIntegral i
      let v = fromIntegral j
      let pos = V2 (u/(fromIntegral w)) (v/(fromIntegral h))
      return . makePixel $ perturb (
        \ca co -> color world (getRay ca co)
                                   ) cam pos jigs

main = do
  n <- getProgName
  writeFile (n++".ppm") $ makePicture 400 200 4
