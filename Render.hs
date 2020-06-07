{-# LANGUAGE UnicodeSyntax #-}

module Render where

import Graphics.Gloss

import Physics
import Tools

render :: World -> Picture
render w = pictures $ map renderParticle $ _particles w

particleRadius :: Double
particleRadius = 20

renderParticle :: Particle -> Picture
renderParticle (Particle x y rot near) =
  pictures
    [ color (particleColor near) $
      translate (dtf x) (dtf y) $ circleSolid (dtf particleRadius)
    , color black $
      translate
        (dtf (x + 0.5 * particleRadius * cos rot))
        (dtf (y + 0.5 * particleRadius * sin rot)) $
      rotate (dtf (360 - rad2deg rot)) $ rectangleSolid (dtf particleRadius) 1
    ]

particleColor :: Int -> Color
particleColor near
  | near > 35 = yellow
  | near > 15 = blue
  | near > 12 = orange
  | otherwise = green
