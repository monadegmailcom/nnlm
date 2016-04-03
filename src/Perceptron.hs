module Perceptron
    ( runIterations,
      getHyperplane
    ) where

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

getHyperplane ::
  Double ->
  (Int -> Double) ->            -- learning rate
  [[Double]] ->
  [[Double]] ->
  (Int, [Double], Double)
getHyperplane eps = (((head . dropWhile p) .) .) . runIterations where
  p (_, _, e) = e >= eps
-- (f .) (g x) = (f .) . g $ x
runIterations ::
  (Int -> Double) ->            -- learning rate
  [[Double]] ->
  [[Double]] ->
  [(Int, [Double], Double)]
runIterations etaF ps1 ps2 = iterate f (1, w0, 1.0 / 0) where
  -- start condition with normalized normal vector
  w0 = take l $  1.0 : 1.0 : repeat 0.0
  l =  1 + length (head ps1)

  -- eval all iterations lazily
  f :: (Int, [Double], Double) -> (Int, [Double], Double)
  f (n, ws_n, _) = (n + 1, ws_n1, e_n1) where
    ws_n1 = smult (1.0 / norm (tail ws)) ws
    ws = vadd ws_n delta
    e_n1 = max (abs (head delta / head ws_n)) (norm (tail delta))
    delta = nextDelta ps1 ps2 (etaF n) ws_n

nextDelta ::
  [[Double]] ->
  [[Double]] ->
  Double ->             -- learning rate
  [Double] ->           -- weights with first element as bias
  [Double]              -- new weight deltas
nextDelta ps1 ps2 eta ws_n = smult alpha $ vsub m1 m2 where
  alpha = eta / fromIntegral (length ps1 + length ps2)
  m1 = condSum isClass2 ps1 -- sum of missclassified class 1 points
  m2 = condSum (not . isClass2) ps2 -- sum of missclassified class 2 points
  condSum p = foldr (f p) nv
  nv = replicate (1 + (length . head) ps1) 0.0

  f p cs as = if p cs then vadd as (1.0 : cs) else as
  isClass2 cs = sprod ws_n (1.0 : cs) < 0.0

  -- scalar product of two vectors
  sprod :: [Double] -> [Double] -> Double
  sprod as = sum . zipWith (*) as

  -- diff of two vectors
  vsub = zipWith (-)
