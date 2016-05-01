module Perceptron
    ( runIterations,
      getHyperplane,
      Iteration(..)
    ) where

import qualified Data.Vector.Unboxed as V (
  Vector, map, replicate, zipWith, cons, sum, head, tail, generate, foldr,
  length, slice)

data RecurPerceptron = RecurPerceptron {
  hyperplane :: [Double],
  leftP      :: Maybe RecurPerceptron,
  rightP     :: Maybe RecurPerceptron
}

-- product of a scalar with a vector
smult :: Double -> [Double] -> [Double]
smult r = map (r *)

-- euclidian vector norm
norm :: [Double] -> Double
norm = sqrt . sum . map (\x -> x * x)

-- sum of two vectors
vadd :: [Double] -> [Double] -> [Double]
vadd = zipWith (+)

smultV :: Double -> V.Vector Double -> V.Vector Double
smultV r = V.map (r *)

subV :: V.Vector Double -> V.Vector Double -> V.Vector Double
subV = V.zipWith (-)

addV :: V.Vector Double -> V.Vector Double -> V.Vector Double
addV = V.zipWith (+)

sprodV :: V.Vector Double -> V.Vector Double -> Double
sprodV as = V.sum . V.zipWith (*) as

-- euclidian vector norm
normV :: V.Vector Double -> Double
normV = sqrt . V.sum . V.map (\x -> x * x)

data Iteration = Iteration { iterationI :: Int
                             , iterationH :: V.Vector Double
                             , iterationE :: Double} deriving (Show)

getHyperplane ::
  Int ->
  Double ->
  (Iteration -> Double) ->
  V.Vector Double ->
  V.Vector Double ->
  Iteration
getHyperplane dim eps etaF ptsA ptsB = head . dropWhile p $ its where
  its = runIterations dim etaF ptsA ptsB
  p itr = iterationE itr >= eps

runIterations ::
  Int ->
  (Iteration -> Double) ->
  V.Vector Double ->
  V.Vector Double ->
  [Iteration]
runIterations dim etaF ptsA ptsB = iterate f $ Iteration 1 w_0 (1.0 / 0.0) where
  w_0 = V.generate (1 + dim) f where
    f 0 = 1
    f 1 = 1
    f _ = 0
  f itr@(Iteration n w_n _) = Iteration (n + 1) w_n1 e_n1 where
    w_n1 = smultV (1.0 / wsn) ws
    ws = addV w_n d_n
    wsn = normV $ V.tail ws
    e_n1 = max (abs (V.head d_n / V.head w_n)) (normV . V.tail $ d_n)
    d_n = nextDelta dim ptsA ptsB (etaF itr) w_n

nextDelta ::
  Int -> -- dim
  V.Vector Double -> -- class A points
  V.Vector Double -> -- class B points
  Double ->
  V.Vector Double ->
  V.Vector Double
nextDelta dim ptsA ptsB eta ws_n = smultV alpha $ subV mA mB where
  alpha = eta / fromIntegral (countA + countB)
  countA = V.length ptsA `div` dim
  countB = V.length ptsB `div` dim
  mA = condSum isClass2 ptsA countA
  mB = condSum (not . isClass2) ptsB countB
  condSum p pts count = foldr (f p) nv [0..count-1] where
    nv = V.replicate (1 + dim) 0.0
    f p i acc = if p v then addV acc v else acc where
      v = 1.0 `V.cons` V.slice (i * dim) dim pts
  isClass2 v = sprodV ws_n v < 0.0
