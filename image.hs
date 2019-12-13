import System.Environment
import Linear
import Control.Lens
import System.Random

type Color = V3 Double

data Ray = Ray
  {orig :: V3 Double
  ,dir  :: V3 Double
  } deriving (Show)

getGens :: (RandomGen g) => g -> [g]
getGens g = let (g1,g2)=split g in g1:getGens g2

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
  }

getRay :: Camera -> V2 Double -> Ray
getRay c (V2 u v) = Ray
  {orig = origin c
  ,dir = lowerLeftCorner c + u*^horizontal c + v*^vertical c - origin c
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

type Raycaster a = a -> Camera -> (V2 Double) -> Color

raycast :: (EMO a) => Raycaster a
raycast o c v = color o (getRay c v)

antialias :: (RandomGen g, EMO a)
          => Raycaster a
          -> Int         -- sqrt number of samples
          -> (Int,Int)   -- resolution of image
          -> g
          -> Raycaster a
antialias r n (w,h) g o c v
  = recip (fromIntegral n) *^ sum $ map (r o c) $ map (v+) $
    take n $ (map (\(x,y)-> V2 x y)) $ zip us vs 
      where
        (g1,g2)=split g
        us = randomRs (0,w') g1
        vs = randomRs (0,h') g2
        w'=recip . fromIntegral $ w
        h'=recip . fromIntegral $ h
    {- [let
    in V2 (x/w') (y/h') 
      | let l=map fromIntegral [0..n-1], x<-l, y<-l] -}

makePicture :: (RandomGen g) => Int -> Int -> Int -> g -> String
makePicture aa w h g = header ++ body
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
      }
    world =
      [Sphere (V3 0 0 (-1)) 0.5
      ,Sphere (V3 0 (-100.5) (-1)) 100
      ]
    body = concat $ do
      j <- map fromIntegral [h-1,h-2..0]
      i <- map fromIntegral [0..w-1]
      let g' = getGens g !! (i+j*h `mod` 10)
      let n = fromIntegral aa
      let jigs = [V2 (k/n)(l/n) | k<-[0..n-1],l<-[0..n-1]]
      let u = fromIntegral i
      let v = fromIntegral j
      let pos = V2 (u/(fromIntegral w)) (v/(fromIntegral h))
      -- return . makePixel $ raycast world cam pos
      return . makePixel $ antialias raycast aa (w,h) g' world cam pos

main = do
  cs<-getArgs
  let [a,w,h]=
        case cs of
          []         -> [1,400,200]
          [a1]       -> [read a1,400,200]
          [a1,a2,a3] -> [read a1,read a2,read a3]
          _          -> error "Supply zero, one, or three arguments."
        :: [Int]
  n <- getProgName
  g <- getStdGen
  let (g1,g2)=split g
  writeFile (n++"_"++show w++"×"++show h++"_aa="++show a++"_1.ppm") $ makePicture a w h g1
  writeFile (n++"_"++show w++"×"++show h++"_aa="++show a++"_2.ppm") $ makePicture a w h g2
