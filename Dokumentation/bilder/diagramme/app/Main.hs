module Main where

import Infrastruktur

import Diagrams.Prelude
import Diagrams.Backend.Postscript

dimensions :: SizeSpec V2 Double
dimensions = dims2D 500.0 500.0

main :: IO ()
-- main = renderSVG "../infrastruktur.svg" dimensions infrastruktur
main =
  renderDia Postscript
  (PostscriptOptions "../infrastruktur.eps" dimensions EPS)
  infrastruktur
