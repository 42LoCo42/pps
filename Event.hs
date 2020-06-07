{-# LANGUAGE UnicodeSyntax #-}

module Event where

import Graphics.Gloss.Interface.IO.Interact

import Physics
import Tools

handleEvent :: Event -> World -> World
handleEvent (EventKey (MouseButton LeftButton) Down _ (kx, ky)) w =
  w
    { _mouseDown = True
    , _particles = Particle (dtf kx) (dtf ky) 0 0 : _particles w
    }
handleEvent (EventKey (MouseButton LeftButton) Up _ _) w =
  w {_mouseDown = False}
handleEvent (EventMotion (kx, ky)) w
  | _mouseDown w =
    w {_particles = Particle (dtf kx) (dtf ky) 0 0 : _particles w}
  | otherwise = w
handleEvent _ w = w
