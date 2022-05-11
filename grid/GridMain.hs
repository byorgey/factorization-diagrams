{-# LANGUAGE FlexibleContexts #-}

import           Data.Colour.SRGB            (Colour, sRGB24)
import           Diagrams.Backend.Rasterific.CmdLine
import           Diagrams.Prelude
import           Factorization

import Data.List.Split (chunksOf)
import           Control.Monad

-- main :: IO ()
-- main = do
--   forM_ ([3^8, 3^9, 3^10] ++ [100, 200 .. 1000]) $ \n ->
--     renderSVG
--       (show n ++ ".svg")
--       cardSize
--       (cardFace' (factorizationBigToSmall n))
--   where
--     cardFace' = bg white . frame 0.1
--          . (beneath (rect 1 1 # lw none)) . sized (dims2D 1 1)
--          . factorDiagram' numerosoColors

-- main :: IO ()
-- main = do
--   forM_ [1..81] $ \n ->
--     renderRasterific
--       (show n ++ ".png")
--       cardSize
--       (card Face n (factorizationBigToSmall n))

-- main :: IO ()
-- main = do
--   renderInfoCard
--   forM_ [1..30] $ \n ->
--     forM_ (zip [1 :: Integer ..] . factorizations $ n) $ \(i,ps) ->
--       forM_ [Face, Back] $ \side ->
--         renderRasterific
--           (show n ++ "-" ++ show i ++ sideTag side ++ ".png")
--           cardSize
--           (card side n ps)

main :: IO ()
main =
  (fdGridList 10 :: Diagram B)
  # bg white
  # defaultMain

-- | @fdGrid n@ creates a grid of factorization diagrams, given a list
--   of lists of integers: the inner lists represent L-R rows, which
--   are laid out from top to bottom.
--
--   > import Diagrams.TwoD.Factorization
--   > fdGridEx = fdGrid [[7,6,5],[4,19,200],[1,10,50]]
--
--   <<diagrams/src_Diagrams_TwoD_Factorization_fdGridEx.svg#diagram=fdGridEx&width=200>>
fdGrid
  :: (Renderable (Path V2 n) b, TypeableFloat n)
  => [[Integer]] -> QDiagram b V2 n Any
fdGrid  = vcat . map hcat . (map . map) (ensquare 1 . factorDiagram . factors)

-- | @fdGridList n@ creates a grid containing the factorization
--   diagrams of all the numbers from @1@ to @n^2@, ordered left to
--   right, top to bottom (like the grid seen on the cover of Hacker
--   Monthly, <http://hackermonthly.com/issue-31.html>).
--
--   > import Diagrams.TwoD.Factorization
--   > grid100 = fdGridList 10
--   > grid100Big = grid100
--
--   <<diagrams/src_Diagrams_TwoD_Factorization_grid100.svg#diagram=grid100&width=400>>
fdGridList
  :: (Renderable (Path V2 n) b, TypeableFloat n)
  => Integer -> QDiagram b V2 n Any
fdGridList n = fdGrid . chunksOf (fromIntegral n) $ [1..n*n]


ensquare
  :: (Renderable (Path V2 n) b, TypeableFloat n)
  => n -> QDiagram b V2 n Any -> QDiagram b V2 n Any
ensquare n d = d # centerXY # sized (dims2D (0.8*n) (0.8*n)) <> square n # lw none

