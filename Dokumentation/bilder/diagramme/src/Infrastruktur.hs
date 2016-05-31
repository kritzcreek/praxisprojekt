{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
module Infrastruktur
    ( infrastruktur
    ) where

import Diagrams.Prelude
import Graphics.SVGFonts

import Data.List as L

text' d s = text s # fontSize d # lw none # fc black

textIt d s = (strokeP $ textSVG' (TextOpts lin2 INSIDE_H KERN False d d) s)
             # lw none # fc black

-- infrastruktur :: Diagram B
infrastruktur =
  let
    images = map (\(text, label, name) -> dockerImage text label # named name)
             [ ("ubuntu", "", "ub")
             , ("fpco/stack-run", "Laufzeitbibliotheken", "fpsr")
             , ("kritzcreeek/stack-kafka-run", "Kafka Client Bibliothek", "kcsr")
             , ("kritzcreeek/produktservice", "Kompilierter Service", "rt")
             , ("kritzcreeek/warenkorbservice", "Kompilierter Service", "wks")
             ]
    arr = connectOutside' (with
                            & arrowHead .~ thorn
                            & lengths   .~ large)
  in
    strutX 1 |||
    (strutY 3
     ===
-- dockerImage :: String -> String -> Diagram B
     vcat (L.intersperse (strutY 6) (take 3 images)) # translate (r2 (19, 0))
     ===
     strutY 6
     ===
     (images !! 3 ||| strutX 3 ||| images !! 4)
     ===
     strutY 0.5)
    # arr "ub" "fpsr"
    # arr "fpsr" "kcsr"
    # arr "kcsr" "rt"
    # arr "kcsr" "wks"
    ||| strutX 1

dockerImage s "" = text' 20 s # translate (r2 (0, -0.8)) <> rect 35.0 8.0 # fc azure
dockerImage s l =
  (text' 17 ("+ " ++ l) # italic
    ===
    strutY 2
    ===
    strokeLine (fromOffsets (replicate 35 unitX)) # translate (r2 (-17.5, 0))
    ===
    strutY 3
    ===
    text' 15 s) # translate (r2 (0, 1.8)) <> rect 35.0 10.0 # fc azure
