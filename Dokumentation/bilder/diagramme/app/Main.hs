module Main where

import Architektur
import Infrastruktur

import Diagrams.Prelude
import Diagrams.Backend.Postscript
import Diagrams.Backend.SVG

dimensions :: SizeSpec V2 Double
dimensions = dims2D 500.0 500.0

main :: IO ()
main = do
  renderDia Postscript
    (PostscriptOptions "../infrastruktur.eps" dimensions EPS)
    infrastruktur

  renderDia Postscript
    (PostscriptOptions "../architektur.eps" dimensions EPS)
    architektur

  -- renderSVG "../architektur.svg" dimensions architektur
