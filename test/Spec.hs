import Perceptron
import Test.Hspec
import Control.Monad
import System.Random (randomR, getStdRandom, setStdGen, mkStdGen)
-- does not build import Graphics.Google.Chart
import Graphics.EasyPlot

getUniformRs ::
  Int -> -- number of random vars
  Double -> -- left bound
  Double -> -- right bound
  IO [Double]
getUniformRs n a b = replicateM n m where
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

prepareTestSample :: IO [(Double, Double)]
prepareTestSample = do
  let r = 0.5
  let w = 0.1
  let n = 100
  let c = (1.0, -1.0)

  xs <- getUniformRs n (r - w / 2) (r + w / 2)
  ps <- getUniformRs n 0.0 pi

  let f = shift c . rotate (0.0,0.0) (pi/4) . pol2Cart
  let bs = zipWith (curry f) xs ps
  return bs

main :: IO ()
main = do
  -- set seed to make sample draw reproducable
  setStdGen $ mkStdGen 42
  ps <- prepareTestSample
  plot X11 [ Data2D [Color Red] [] ps]
  hspec $
    describe "generate sample data" $
      it "2 d arcs"
        pending
