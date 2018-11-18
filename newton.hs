
module NewtonianMethod ( newtonMethod, mapFractal)

where

import Data.Complex

type ComplexFunction = (Complex Double -> Complex Double)

mapFractal :: ComplexFunction -> ComplexFunction -> (Double, Double) -> (Double, Double) -> (Int, Int) -> Int -> Double -> [[(Complex Double, Int)]]
mapFractal f f' (bx,by) (fracW, fracH) (pixW, pixH) inters eps =  
    [[(nM ((bx + (fromInt x) * (fracW / (fromInt pixW))) :+ (by + (fromInt y) * (fracH / (fromInt pixH)))))
    | x <- [0..pixW-1] ] | y <- [0..pixH - 1]] 
  where 
        nM comDouble = newtonMethod comDouble f f' inters eps 
        fromInt = fromIntegral

newtonMethod :: Complex Double-> ComplexFunction -> ComplexFunction -> Int -> Double -> (Complex Double,Int)
newtonMethod z0 _ _ 0 _ = (z0,0)
newtonMethod z0 f f' ite threshold =
  let z =  (z0 - ((f z0) / (f' z0))) *(1:+0)
      delta = abs $ z - z0
  in
      if (realPart delta < threshold) && (imagPart delta < threshold) then (z,ite) else newtonMethod z f f' (ite - 1) threshold
