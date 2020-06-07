{-# LANGUAGE UnicodeSyntax #-}

module Physics where

import Tools

data Particle =
  Particle
    { _x :: !Double
    , _y :: !Double
    , _rot :: !Double
    , _near :: !Int
    }

data World =
  World
    { _particles :: ![Particle]
    , _alpha :: Double
    , _beta :: Double
    , _v :: Double
    , _rad :: Double
    , _mouseDown :: !Bool
    }

update :: World -> World
update w = w {_particles = map (updateParticle w) $ _particles w}

updateParticle :: World -> Particle -> Particle
updateParticle w p =
  p
    { _x = _x p + cos rot * v
    , _y = _y p + sin rot * v
    , _rot = rot
    , _near = nearC
    }
  where
    ps = _particles w
    near = filter (\p2 -> pDist p p2 <= _rad w) ps
    nearC = length near
    most = toEnum $ sum $ map (hemisphere p) near
    alpha = _alpha w
    beta = _beta w
    v = _v w
    rot = norm $ _rot p + alpha + beta * toEnum nearC * signum most

pDist :: Particle -> Particle -> Double
pDist p1 p2 = sqrt (dx ** 2 + dy ** 2)
  where
    dx = _x p1 - _x p2
    dy = _y p1 - _y p2

hemisphere :: Particle -> Particle -> Int
hemisphere center near
  | rel < pi = 1 -- left
  | rel > pi = -1 -- right
  | otherwise = 0
  where
    rel =
      norm $
      angleBetween2P (_x center) (_y center) (_x near) (_y near) - _rot center
