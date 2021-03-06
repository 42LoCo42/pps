module Tools where

arc :: Floating a => a
arc = pi / 180

deg2rad :: Floating a => a -> a
deg2rad = (* arc)

rad2deg :: Floating a => a -> a
rad2deg = (/ arc)

angleBetween2P :: (Ord a, Floating a) => a -> a -> a -> a -> a
angleBetween2P x1 y1 x2 y2
  | dx < 0 = raw + pi
  | dy < 0 = raw + pi * 2
  | otherwise = raw
  where
    raw = atan $ dy / dx
    dx = x2 - x1
    dy = y2 - y1

norm :: Double -> Double
norm a = a - tp * toEnum (floor (a / tp))
  where
    tp = 2 * pi

dtf :: (Real a, Fractional b) => a -> b
dtf = fromRational . toRational
