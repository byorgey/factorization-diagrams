module Backdrop where

import           Diagrams.Backend.Rasterific
import           Diagrams.Backend.Rasterific.CmdLine
import           Diagrams.Prelude

import           Data.List.Split

import           Cards
import           Factorization

backdrop
  = [1..30]
  # map (\n -> card Face n (head . factorizations $ n))
  # chunksOf 10
  # map (hsep 0.2)
  # vsep 0.2
  # bg white

main :: IO ()
main =
 renderRasterific
   ("backdrop.png")
   (dims2D 1600 600)
   backdrop
