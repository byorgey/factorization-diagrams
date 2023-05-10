module Cards where

import           Diagrams.Backend.Rasterific
import           Diagrams.Backend.Rasterific.CmdLine
import           Diagrams.Prelude
import           Factorization
import           Graphics.SVGFonts

import           Control.Monad                          (forM_)
import           Data.Bifunctor                         (first)
import           Data.Char                              (toLower)
import           Data.List                              (intercalate, nub,
                                                         permutations, sortBy)
import           Data.List.Split
import           Data.Ord                               (Down (..), comparing)
import           Math.NumberTheory.Primes               (factorise, unPrime)

import           System.IO.Unsafe

lin' = unsafePerformIO lin

text' :: Double -> String -> Diagram B
text' d t = t
  # svgText def
  # fit_height d
  # set_envelope
  # lw none # fc black

factorizations :: Integer -> [[Integer]]
factorizations
  = nub . permutations
  . concatMap (uncurry (flip (replicate . fromIntegral)))
  . map (first unPrime)
  . factorise

factorizationBigToSmall :: Integer -> [Integer]
factorizationBigToSmall
  = concatMap (uncurry (flip (replicate . fromIntegral)))
  . sortBy (comparing (Down . fst))
  . map (first unPrime)
  . factorise

cardFace :: [Integer] -> Diagram B
cardFace = bg white . frame 0.1
         . (beneath (rect 1 1 # lw none)) . sized (dims2D 1 1)
         . factorDiagram

cardBack :: Integer -> [Integer] -> Diagram B
cardBack n ps
  | length ps <= 1 = (<> square 1 # lw none) . text' 0.8 . show $ n
  | otherwise
  = vsep 0.1
    [ text' 0.6 (show n)
    , text' 0.3 (intercalate "×" (map show ps))
    ]
    # centerY
    <> square 1 # lw none

data Side = Face | Back
  deriving Show

card :: Side -> Integer -> [Integer] -> Diagram B
card Face _ ps = cardFace ps
card Back n ps = cardBack n ps

sideTag :: Side -> String
sideTag s = "[" ++ (map toLower . show $ s) ++ "]"

cardSize :: SizeSpec V2 Double
cardSize = dims2D 1125 1125
-- cardSize = dims2D 400 400

cardSizeSmall :: SizeSpec V2 Double
cardSizeSmall = dims2D 400 400

renderInfoCard = do
  forM_ [Face, Back] $ \side ->
    renderRasterific
      ("info" ++ sideTag side ++ ".png")
      cardSize
      (infoCard side # frame 0.2)
  where
    infoCard Face = vsep 0.1 . map centerX $
      [ "© Brent Yorgey 2016" # text' 0.2
      , "CC BY 3.0 license" # text' 0.15
      , "mathlesstraveled.com/factorization" # text' 0.15
      , "rev 2, 3.12.16" # text' 0.15
      ]
    infoCard Back = mempty

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
-- main = [1..30]
--   # concatMap (\n -> map (card Face n) (factorizations n))
--   # chunksOf 7
--   # map (hsep 0.2)
--   # vsep 0.2
--   # sized cardSize
--   # centerXY
--   # beneath (rect 1125 1125 # lw none # fc white)
--   # renderRasterific "allCardsGrid.png" cardSize
