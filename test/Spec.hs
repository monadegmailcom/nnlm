import Perceptron
import Test.Hspec (hspec, describe, it, pending)
import Control.Monad (replicateM)
import System.Random (randomR, getStdRandom, setStdGen, mkStdGen)
-- does not build import Graphics.Google.Chart
import Graphics.EasyPlot

import Data.Vector.Unboxed ((!))
import qualified Data.Vector.Unboxed as V
  (Vector, replicateM, length, unfoldrN, tail, head, null,
   unzip, zipWith, toList, take, slice)

getUniformRs ::
  Int -> -- number of random vars
  Double -> -- left bound
  Double -> -- right bound
  IO (V.Vector Double)
getUniformRs n a b = V.replicateM n m where
  m = getStdRandom $ randomR (a, b)

shift ::
  (Double, Double) -> -- shift
  (Double, Double) -> -- point
  (Double, Double)
shift (sx, sy) (x, y) = (sx + x, sy + y)

rotate ::
  (Double, Double) -> -- rotation center
  Double ->           -- rotation angle
  (Double, Double) -> -- point
  (Double, Double)
rotate (cx, cy) phi (x, y) =
  (x' * cos phi - y' * sin phi + cx,
   y' * cos phi + x' * sin phi + cy) where
     x' = x - cx
     y' = y - cy

scale ::
  Double -> -- scale factor
  (Double, Double) -> -- point
  (Double, Double)
scale c (x, y) = (c * x, c * y)

pol2Cart ::
  (Double, Double) -> -- radius, phi
  (Double, Double)
pol2Cart (r, phi) = (r * cos phi, r * sin phi)

interleave  :: ((Double, Double) -> [Double])
            -> V.Vector Double
            -> V.Vector Double
            -> V.Vector Double
interleave f as bs = V.unfoldrN (2 * V.length as) g seed where
  seed = ([], as, bs)
  g (vs, as, bs)
    | V.null as || V.null bs = Nothing
    | null vs = Just (x, (xs, V.tail as, V.tail bs))
    | otherwise = Just (head vs, (tail vs, as, bs))
    where x:xs = f (V.head as, V.head bs)

sample :: Double
         -> Double
         -> Int
         -> (Double, Double)
         -> (Double, Double)
         -> IO (V.Vector Double)
sample r w n c (phi1, phi2) = do
    rs <- getUniformRs n (r - w / 2) (r + w / 2)
    ps <- getUniformRs n phi1 phi2
    return $ interleave f rs ps
  where
    f = toList . shift c . rotate (0.0,0.0) (pi/4) . pol2Cart
    toList (x, y) = [x, y]

toPairs :: V.Vector Double -> [(Double, Double)]
toPairs v = foldr f [] [0..n] where
  n = (V.length v `div` 2) - 1
  f i acc = toPair (V.slice (i * 2) 2 v) : acc
  toPair v = (v!0, v!1)

main :: IO ()
main = do
  -- set seed to make sample draw reproducable
  setStdGen $ mkStdGen 42

  ptsA <- sample 0.5 0.1 100 (1.0, -1.0) (0, pi)
  ptsB <- sample 0.5 0.1 100 (1.5, -1.0) (pi, 2 * pi)

  let etaF = const 1.0

  print "runIterations"
  let len = 15
  let ws = take len $ runIterations 2 etaF ptsA ptsB
  print ws
  print "Hyperplane"
  let hyperplane = getHyperplane 2 0.1 etaF ptsA ptsB
  print hyperplane

  let hp (Iteration _ v _) x = -(v!0 + v!1 * x) / v!2
  let hs = [Data2D [Title "Class Red",  Color Red]  [] (toPairs ptsA),
            Data2D [Title "Class Blue", Color Blue] [] (toPairs ptsB)]
  plot X11 $ (Function2D [Title "", Color Black] [Range 0.6 1.8] . hp $ hyperplane) : hs
  let pts = map (\i -> (fromIntegral i, iterationE (ws!!i))) [0..len-1]
  plot X11 $ Data2D [Title "Rel. error", Style Lines, Color Black] [] pts

  hspec $
    describe "generate sample data" $
      it "2 d arcs"
        pending
