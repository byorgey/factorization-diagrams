module GameLogo where

import           Diagrams.Backend.Rasterific
import           Diagrams.Backend.Rasterific.CmdLine
import           Diagrams.Prelude

import           Data.List.Split

import           Cards
import           Factorization

gameLogo :: Diagram B
gameLogo = mconcat
  [ factorDiagram [2,3] # rotateBy (1/4)
    # frame 0.2
    # opacity 0.2
  , text' 1.5 "Factorization diagrams!"
  ]
  # sized (dims2D 350 150)
  # beneath (rect 350 150 # lw none)
  # bg white

main :: IO ()
main =
 renderRasterific
   ("gameLogo.png")
   (dims2D 350 150)
   gameLogo
