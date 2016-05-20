module Main where

import Infrastruktur

import Diagrams.Prelude
import Diagrams.Backend.SVG

dimensions :: SizeSpec V2 Double
dimensions = dims2D 400.0 500.0

main :: IO ()
main = renderSVG "../infrastruktur.svg" dimensions infrastruktur
