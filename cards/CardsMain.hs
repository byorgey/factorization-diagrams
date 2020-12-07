import           Cards
import           Data.Colour.SRGB            (Colour, sRGB24)
import           Diagrams.Backend.Rasterific
import           Diagrams.Prelude
import           Factorization

import           Control.Monad

main :: IO ()
main = do
  forM_ [1 .. 9] $ \m ->
    forM_ [1 .. 9] $ \n ->
      renderRasterific
        (show m ++ "x" ++ show n ++ ".png")
        cardSize
        (cardFace' (factorizationBigToSmall m ++ factorizationBigToSmall n))

  where
    cardFace' = bg white . frame 0.1
         . (beneath (rect 1 1 # lw none)) . sized (dims2D 1 1)
         . factorDiagram' numerosoColors

numerosoColors :: [Colour Double]
numerosoColors =
  [ black
  , sRGB24 6 71 151
  , sRGB24 41 184 206
  , sRGB24 252 200 65
  , gray
  , sRGB24 0 174 160
  , gray
  , sRGB24 238 115 48
  , gray
  , gray
  ]


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

-- main :: IO ()
-- main = [1..50]
--   # map (\n -> card Face n (head . factorizations $ n))
--   # chunksOf 10
--   # map (hsep 0.2)
--   # vsep 0.2
--   # bg white
--   # defaultMain
