{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
module Architektur
  ( architektur
  ) where

import Diagrams.Prelude
import Diagrams.Backend.Postscript
import Graphics.SVGFonts
import Data.Colour.Palette.BrewerSet

import Data.List as L

data Thing
  = Component Double Double String String
  | Database Double Double String String
  | Label Double Double String
  | Rect Double Double Double Double

sky = brewerSet Spectral 11 !! 7
bright = brewerSet Spectral 11 !! 5
dbColor = brewerSet Spectral 11 !! 8

positions = [ Component 0 2 "Produktservice" "ps"
            , Component 2 4 "Kafka" "k"
            , Component 4 2 "Warenkorbservice" "wk"
            , Database  0 0 "Produkt DB" "pdb"
            , Database  4 0 "Warenkorb DB" "wdb"
            , Label 0.4 3 "Publish"
            , Label 3.7 3 "Subscribe"
            , Rect 0 1 2.7 3.6
            , Rect 4 1 2.7 3.6
            , Rect 2.1 4.1 2.7 1.8
            ]

architektur :: Diagram B
architektur =
  foldMap component positions
  # frame 0.1
  # connectOutside "ps" "k"
  # connectOutside "wk" "k"
  # connectOutside "ps" "pdb"
  # connectOutside "wk" "wdb"

component :: Thing -> Diagram B
component t = case t of
  Component x y title name ->
    translate (V2 x y) $
      text title # fontSize 16 <> stack # fc bright # named name
  Database x y title name ->
    translate (V2 x y) $
      text title # fontSize 15 <> database # translateY 0.1 # fc dbColor # named name
  Label x y s ->
    translate (V2 x y) $ text s # fontSize 18
  Rect x y w h ->
    translate (V2 x y) $ rect w h # fc sky

database :: Diagram B
database =
  circle 0.75 # scaleY 0.3 # translate (V2 0 0.5)
  <> rect 1.5 1

stack :: Diagram B
stack =
  rect 2 1
  <> rect 2 1 # translate (V2 0.1 0.1)
  <> rect 2 1 # translate (V2 0.2 0.2)
