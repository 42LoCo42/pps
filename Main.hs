{-# LANGUAGE UnicodeSyntax #-}

module Main where

import Graphics.Gloss

import Event
import Physics
import Render
import Tools

world :: World
world =
  World
    { _particles = []
    , _alpha = deg2rad 180
    , _beta = deg2rad 17
    , _v = 16.75
    , _rad = 125
    , _mouseDown = False
    }

main :: IO ()
main = play FullScreen black 60 world render handleEvent (const update)
