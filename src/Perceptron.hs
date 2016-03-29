module Perceptron
    ( RecurPerceptron(..),
      Iteration(..),
      nextIteration
    ) where

data RecurPerceptron = RecurPerceptron {
  hyperplane :: [Double],
  leftP      :: Maybe RecurPerceptron,
  rightP     :: Maybe RecurPerceptron
}

data Iteration = Iteration {
  weights :: [Double],
  bias :: Double,
  learningRateF :: Integer -> Double,
  n :: Integer
}

nextIteration :: Iteration -> Iteration
nextIteration it = undefined
