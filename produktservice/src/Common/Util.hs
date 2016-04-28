module Common.Util where

import Data.Char (toLower)

fieldLabelDrop :: Int -> String -> String
fieldLabelDrop n s = let (x:xs) = drop n s in toLower x : xs
