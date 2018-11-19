
module NewtonianMethod ( newtonMethod, mapFractal, generateImage)

where

import Data.Complex
import Control.Parallel
import Control.DeepSeq
type ComplexFunction = (Complex Double -> Complex Double)


mapFractal :: ComplexFunction -> ComplexFunction -> (Double, Double) -> (Double, Double) -> (Int, Int) -> Int -> Double -> [[(Complex Double, Int)]]
mapFractal f f' (bx,by) (fracW, fracH) (pixW, pixH) inters eps =
    [ [ nM (((bx + (fracXAtX x))) :+ (by + (fracYAtY y))) | x <- [0..pixW-1] ] | y <- [0..pixH - 1]]
  where
        nM comDouble = newtonMethod comDouble f f' inters eps
        toD = fromIntegral
        fracXAtX x= (toD x*) $ fracW / (toD pixW)
        fracYAtY y= (toD y*) $ fracH / (toD pixH)

generateImage :: ComplexFunction -> ComplexFunction -> (Double,Double) -> (Double,Double) -> (Int,Int) -> Int -> Double -> [[(Complex Double, Int)]]
generateImage f f' (minfX,minfY) (fTotalW,fTotalH) (pixTotalW,pixTotalH) inters eps = concat $ testf (nmOnBox) [  (x,y) |x <- [0..(xBoxes-1)] , y <- [0..(yBoxes-1)] ]
  where nmOnBox (x,y) = mapFractal f f' (minfX + fracXAtX x ,minfY + fracYAtY y) (fTotalW / (toD xBoxes) , fTotalH / (toD yBoxes)) (pixTotalW `div` xBoxes , pixTotalH `div` yBoxes) inters eps
        toD = fromIntegral
        fracXAtX x= (toD (x * xStep)*) $ fTotalW / (toD pixTotalW)
        fracYAtY y= (toD (y * yStep)*) $ fTotalH/ (toD pixTotalH)
        (xBoxes,yBoxes) = (4,4)
        xStep = pixTotalH `div` xBoxes
        yStep = pixTotalW `div` yBoxes

testf :: ((Int,Int) -> [[(Complex Double,Int)]]) -> [(Int,Int)] -> [[[(Complex Double, Int)]]]
testf f [x] = [f x]
testf f (x:xs) = (ef) `par` (ef : etestf)
  where ef = force (f x)
        etestf = testf f xs

newtonMethod :: Complex Double-> ComplexFunction -> ComplexFunction -> Int -> Double -> (Complex Double,Int)
newtonMethod z0 _ _ 0 _ = (z0,0)
newtonMethod z0 f f' ite threshold =
  let z =  (z0 - ((f z0) / (f' z0))) *(1:+0)
      delta = abs $ z - z0
  in
      if (realPart delta < threshold) && (imagPart delta < threshold) then (z,ite) else newtonMethod z f f' (ite - 1) threshold
