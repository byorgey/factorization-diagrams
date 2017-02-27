module ShopAd where

import           Diagrams.Backend.Rasterific
import           Diagrams.Backend.Rasterific.CmdLine
import           Diagrams.Prelude

import           Data.List.Split

import           Cards
import           Factorization

shopAd :: Diagram B
shopAd =
  factorDiagram [2,3] # rotateBy (1/4)
  # frame 0.2
  # sized (dims2D 180 125)
  # beneath (rect 180 125 # lw none)
  # bg white

main :: IO ()
main =
 renderRasterific
   ("shopAd.png")
   (dims2D 180 125)
   shopAd
