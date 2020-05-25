module Render where

import Graphics.Gloss

import Physics
import Tools

render :: World -> Picture
render w = pictures $ map renderParticle $_particles w

particleRadius :: Float
particleRadius = 20

renderParticle :: Particle -> Picture
renderParticle (Particle x y rot near) = pictures
  [ color (particleColor near) $ translate x y $ circleSolid particleRadius
  , color black $ translate
    (x + 0.5 * particleRadius * cos rot)
    (y + 0.5 * particleRadius * sin rot) $
    rotate (360 - rad2deg rot) $ rectangleSolid particleRadius 1
  ]

particleColor :: Int -> Color
particleColor near
  | near > 35 = yellow
  | near > 15 = blue
  | near > 12 = orange
  | otherwise = green
