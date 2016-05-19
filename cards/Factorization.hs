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
  | otherwise
    = mconcat
      [ mconcat
        [ circle r
        , circle (0.8 * r) # reversePath
        ]
        # strokeP
        # fc (colors !! fromIntegral (p `div` 10))
      , circle (0.6 * r)
        # fc (colors !! fromIntegral (p `mod` 10))
      ]
      # lw none
  where
    poly = polygon (with & polyType   .~ PolyRegular (fromIntegral p) r
                         & polyOrient .~ OrientH
                   )

-- | A default set of digit colors, based very loosely on the color
--   code for resistors (<http://en.wikipedia.org/wiki/Electronic_color_code>),
--   lightened up a bit by blending with white.
--
--   <<diagrams/src_Diagrams_TwoD_Factorization_showDefaultColors.svg#diagram=showDefaultColors&height=50>>
defaultColors :: [Colour Double]
defaultColors = map (blend 0.1 white)
  [black,red,orange,yellow,green,blue,gray,purple,white,brown]

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
factorDiagram = centerXY . foldr (primeLayout defaultColors) (circle 1 # fc black # lw none)

factors :: Integer -> [Integer]
factors 1 = []
factors n = maybe [n] (\a -> a : factors (n `div` a)) mf
  where
    mf = listToMaybe $ filter (\x -> (n `mod` x) == 0) [2 .. n - 1]
    -- only need to go to @intSqrt n@ really
