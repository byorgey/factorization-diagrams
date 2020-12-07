{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) 2012, 2016 Brent Yorgey
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  byorgey@gmail.com
--
-- Factorization diagrams, as seen at
-- <http://mathlesstraveled.com/2012/10/05/factorization-diagrams/>
-- and
-- <http://mathlesstraveled.com/2012/11/05/more-factorization-diagrams/>
-- and on the cover of Hacker Monthly
-- (<http://hackermonthly.com/issue-31.html>): visually represent the
-- prime factorization of n by drawing n dots recursively grouped
-- according to the factors.
--
-- This version of the file has been specifically tweaked for making a
-- set of factorization diagram cards.
--
-----------------------------------------------------------------------------

module Factorization where

import           Data.Char        (digitToInt)
import           Data.List.Split  (chunksOf)
import           Data.Maybe       (listToMaybe)
import           Diagrams.Prelude

import           Data.Colour.SRGB (sRGB24)

-- | @primeLayout@ takes a positive integer p (the idea is for it to
--   be prime, though it doesn't really matter) and a diagram, and lays
--   out p rotated copies of the diagram in a circular pattern.
--
--   There is a special case for @p = 2@: if the given diagram is taller
--   than it is wide, then the two copies will be placed beside each
--   other; if wider then tall, they will be placed one above the
--   other.
--
--   The regular @p@-gon connecting the centers of the laid-out
--   diagrams is also filled in with vertical bars of color
--   representing the number @p@.  In particular, there is one color
--   for each decimal digit (the provided list should have length 10
--   and represents the digits 0-9), and the colors, read left to
--   right, give the decimal expansion of @p@.
--
--   > import Diagrams.TwoD.Factorization
--   > plExample
--   >   = pad 1.1 . centerXY
--   >   . hsep 0.5
--   >   . map (sized (mkWidth 1))
--   >   $ [ primeLayout defaultColors 5 (circle 1 # fc black)
--   >     , primeLayout defaultColors 103 (square 1 # fc green # lw none)
--   >     , primeLayout (repeat white) 13 (circle 1 # lc orange)
--   >     ]
--
--   <<diagrams/src_Diagrams_TwoD_Factorization_plExample.svg#diagram=plExample&width=400>>
primeLayout :: (Renderable (Path V2 n) b, TypeableFloat n)
            => [Colour Double] -> Integer -> QDiagram b V2 n Any -> QDiagram b V2 n Any
primeLayout colors 2 d
  | width d >= height d
  = vcat [ d
         , strutY (height d / 3)
           <> rect (lf * width d) (wf * height d) # lw none # fc (colors !! 2)
         , d # reflectY
         ]
    # centerY
  | otherwise
  = hcat [ d
         , strutX (width d / 3)
           <> rect (wf * width d) (lf * height d) # lw none # fc (colors !! 2)
         , d # reflectX
         ]
    # centerX
  where
    lf = 2/3
    wf = 1/10

primeLayout colors p d
  = (mconcat $
       map (\n -> d # translateY r # rotateBy
              (fromIntegral n/fromIntegral p)) [0..p-1]
    )
    <>
    primeThing colors p r # scale 0.5

  where w  = max (width d) (height d)
        r  = w * c / sin (tau / (2 * fromIntegral p))
        c  = 0.75

primeThing
  :: (Renderable (Path V2 n) b, TypeableFloat n)
  => [Colour Double] -> Integer -> n -> QDiagram b V2 n Any
primeThing colors p r
  | p < 10 = strokeP poly
             # fc (colors!!(fromIntegral p `mod` 10))
             # lw none
             # if (p == 3) then rotateBy (1 / (fromIntegral p * 2)) else id
  | otherwise
    = mconcat
      [ ring (fromIntegral (p `div` 10))
      , unitThing colors (p `mod` 10) (0.6 * r)
      ]
      # lw none
  where
    poly = polygon (with & polyType   .~ PolyRegular (fromIntegral p) r
                         & polyOrient .~ OrientH
                   )
    ring 1 = mconcat
      [ circle r
      , circle (0.8 * r) # reversePath
      ]
      # strokeP
      # fc (colors !! 1)
    ring n = mconcat . iterateN n (rotate nTurn) $ seg
      where
        gap = 1/30 @@ turn
        nTurn = 1/fromIntegral n @@ turn
        seg =
          annularWedge r (0.8*r)
            (yDir # rotate (0.5 *^ gap))
            (nTurn ^-^ gap)
          # strokeP
          # fc (colors !! fromIntegral n)


unitThing colors 1 r = circle r # fc (colors !! 1) # lw none
unitThing colors p r
  | p `elem` [3, 7]  = primeThing colors p r
unitThing colors 9 r = (centerX (t ||| t) === t) # alignY (1/3)
  where
    t = primeThing colors 3 (r/2)
unitThing _      n _ = error $ "Impossible: prime ending with " ++ show n

-- | A default set of digit colors, based very loosely on the color
--   code for resistors (<http://en.wikipedia.org/wiki/Electronic_color_code>),
--   lightened up a bit by blending with white.
--
--   <<diagrams/src_Diagrams_TwoD_Factorization_showDefaultColors.svg#diagram=showDefaultColors&height=50>>
defaultColors :: [Colour Double]
defaultColors =
  [ black
  , krzywinskiColors !! 7    -- purple
  , krzywinskiColors !! 2    -- light blue
  , krzywinskiColors !! 4    -- yellow
  , gray
  , krzywinskiColors !! 5    -- dark blue
  , gray
  , krzywinskiColors !! 1    -- orange
  , gray
  , gray
  ]

-- An old color scheme
--
-- colorScheme :: [Colour Double]
-- colorScheme = [ sRGB24 0xff 0x0d 0x00
--               , sRGB24 0xff 0x7c 0x00
--               , sRGB24 0x03 0x8f 0xa9
--               , sRGB24 0x00 0xcc 0x19
--               ]

-- Generated using coolers.co,
-- https://coolors.co/a4036f-0061ff-33a1fd-fdca40-f78800
colorScheme :: [Colour Double]
colorScheme = [ flirt
              , brandeisBlue
              , brilliantAzure
              , sunglow
              , tangerine
              ]
  where
    flirt          = sRGB24 0xa4 0x03 0x6f
    brandeisBlue   = sRGB24 0x00 0x61 0xff
    brilliantAzure = sRGB24 0x33 0xa1 0xfd
    sunglow        = sRGB24 0xfd 0xca 0x40
    tangerine      = sRGB24 0xf7 0x88 0x00

-- | These colors are chosen to be easily distinguishable to people
--   with the three most common forms of color blindness (deteranopia,
--   protanopia, tritanopia).  http://mkweb.bcgsc.ca/colorblind/
krzywinskiColors :: [Colour Double]
krzywinskiColors =
  [ black
  , sRGB24 230 159   0   -- orange
  , sRGB24  86 180 233   -- sky blue
  , sRGB24   0 158 115   -- bluish green
  , sRGB24 240 228  66   -- yellow
  , sRGB24   0 114 178   -- blue
  , sRGB24 213  94   0   -- vermillion
  , sRGB24 204 121 167   -- reddish purple
  ]

-- | Create a centered factorization diagram from the given list of
--   factors (intended to be primes, but again, any positive integers
--   will do; note how the below example uses 6), by recursively
--   folding according to 'primeLayout', with the 'defaultColors' and
--   a base case of a black circle.
--
--   > import Diagrams.TwoD.Factorization
--   > factorDiagram'Ex = factorDiagram' [2,5,6]
--
--   <<diagrams/src_Diagrams_TwoD_Factorization_factorDiagram'Ex.svg#diagram=factorDiagram'Ex&height=200>>
factorDiagram :: (Renderable (Path V2 n) b, TypeableFloat n)
               => [Integer] -> QDiagram b V2 n Any
factorDiagram = factorDiagram' defaultColors

factorDiagram' :: (Renderable (Path V2 n) b, TypeableFloat n)
               => [Colour Double] -> [Integer] -> QDiagram b V2 n Any
factorDiagram' colors = centerXY . foldr (primeLayout colors) (circle 1 # fc (colors !! 1) # lw none)

factors :: Integer -> [Integer]
factors 1 = []
factors n = maybe [n] (\a -> a : factors (n `div` a)) mf
  where
    mf = listToMaybe $ filter (\x -> (n `mod` x) == 0) [2 .. n - 1]
    -- only need to go to @intSqrt n@ really
