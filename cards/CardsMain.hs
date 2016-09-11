module CardsMain where

import           Cards

main :: IO ()
main = do
  renderInfoCard
  forM_ [1..30] $ \n ->
    forM_ (zip [1 :: Integer ..] . factorizations $ n) $ \(i,ps) ->
      forM_ [Face, Back] $ \side ->
        renderRasterific
          (show n ++ "-" ++ show i ++ sideTag side ++ ".png")
          cardSize
          (card side n ps)

-- main :: IO ()
-- main = [1..50]
--   # map (\n -> card Face n (head . factorizations $ n))
--   # chunksOf 10
--   # map (hsep 0.2)
--   # vsep 0.2
--   # bg white
--   # defaultMain
