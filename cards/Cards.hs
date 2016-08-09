import           Diagrams.Backend.Rasterific
import           Diagrams.Backend.Rasterific.CmdLine
import           Diagrams.Prelude
import           Factorization
import           Graphics.SVGFonts

import           Control.Monad                          (forM_)
import           Data.Char                              (toLower)
import           Data.List                              (intercalate, nub,
                                                         permutations)
import           Data.List.Split
import           Math.NumberTheory.Primes.Factorisation (factorise)

text' :: Double -> String -> Diagram B
text' d t = stroke (textSVG' (TextOpts lin INSIDE_H KERN False d d) t) # fc black

factorizations :: Integer -> [[Integer]]
factorizations = nub . permutations
               . concatMap (uncurry (flip replicate)) . factorise

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

renderInfoCard = do
  forM_ [Face, Back] $ \side ->
    renderRasterific
      ("info" ++ sideTag side ++ ".png")
      cardSize
      (infoCard side # frame 0.1)
  where
    infoCard Face = vsep 0.1 . map centerX $
      [ "© Brent Yorgey 2016" # text' 0.2
      , "CC 3.0-Attribution" # text' 0.15
      , "mathlesstraveled.com/factorization" # text' 0.15
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

main :: IO ()
main = [1..50]
  # map (\n -> card Face n (head . factorizations $ n))
  # chunksOf 10
  # map (hsep 0.2)
  # vsep 0.2
  # bg white
  # defaultMain
