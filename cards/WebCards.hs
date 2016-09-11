{-# LANGUAGE FlexibleContexts #-}

module WebCards where

import           Control.Monad
import           Data.List.Split

import           Diagrams.Backend.Rasterific.CmdLine
import           Diagrams.Prelude

import           Cards
import           Factorization

theCards = [6, 7, 29, 30]

webCards :: Diagram B
webCards =
  [ webCard n side # mkWebCard
  | n <- theCards, side <- [Face, Back] ]
  # chunksOf 4
  # map (hsep 10)
  # vsep 10
  # frame 5

mkWebCard d = d
 # sized (dims2D 100 100)
 # frame 10
 # sized (dims2D 100 100)
 # beneath (roundedRect 100 100 10 # fc white)

webCard n Face = factorDiagram (head $ factorizations n)
webCard n Back = cardBack n (head $ factorizations n)

main :: IO ()
main = mainWith [ ("webCards", webCards)
                , ("6f", webCard 6 Face # mkWebCard # frame 5)
                , ("6b", webCard 6 Back # mkWebCard # frame 5)
                ]
