import           Data.List
import           Math.NumberTheory.Primes.Factorisation

counts = map (length . concat . map (nub . permutations . concatMap (uncurry (flip replicate)) . factorise)) [[1..30]]
