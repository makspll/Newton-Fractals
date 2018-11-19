
module NewtonianMethod ( newtonMethod, mapFractal, generateImage)

where

import Data.Complex
import Control.Parallel
import Control.DeepSeq
import Data.Word
import FractalSettings
import qualified ByteString.StrictBuilder as BSBS
import qualified Data.ByteString as BS
import Colouring

mapFractal :: ComplexFunction -> ComplexFunction -> FractalSettings -> (Double, Double) -> (Double, Double) -> (Int, Int) -> Int -> Double -> BSBS.Builder
mapFractal f f' fs (bx,by) (fracW, fracH) (pixW, pixH) inters eps =  mconcat [nM (((bx + (fracXAtX x))) :+ (by + (fracYAtY y))) | x <- [0..pixW-1] , y <- [0..pixH - 1]]
  where
        nM comDouble = applyColourFunc fs $ newtonMethod comDouble f f' inters eps
        toD = fromIntegral
        fracXAtX x= (toD x*) $ fracW / (toD pixW)
        fracYAtY y= (toD y*) $ fracH / (toD pixH)

generateImage :: ComplexFunction -> ComplexFunction -> FractalSettings -> BS.ByteString
generateImage f f' fs =BSBS.builderBytes $ testf (nmOnBox) [(x,y) |x <- [0..(xBoxes-1)] , y <- [0..(yBoxes-1)]]
  where nmOnBox (x,y) = mapFractal f f' fs (minfX + fracXAtX x ,minfY + fracYAtY y) (fTotalW / (toD xBoxes) , fTotalH / (toD yBoxes)) (pixTotalW `div` xBoxes , pixTotalH `div` yBoxes) inters eps
        toD = fromIntegral
        (minfX, minfY, fTotalW,fTotalH) = (fsXBound2 fs, fsYBound2 fs,(fsXBound1 fs) - (fsXBound2 fs),(fsYBound1 fs - fsYBound2 fs))
        (pixTotalW,pixTotalH,inters,eps) =(fsWid fs, fsHei fs,fsIters fs,fsEpsilon fs)
        fracXAtX x= (toD (x * xStep)*) $ fTotalW / (toD pixTotalW)
        fracYAtY y= (toD (y * yStep)*) $ fTotalH/ (toD pixTotalH)
        (xBoxes,yBoxes) = (1,2) -- keep X at 1 otherwise will not merge correctly
        xStep = pixTotalH `div` xBoxes
        yStep = pixTotalW `div` yBoxes

testf :: ((Int,Int) -> BSBS.Builder) -> [(Int,Int)] -> BSBS.Builder
testf f [x] = f x
testf f (x:xs) = (ef) `par` ((<>)ef etestf)
  where ef = (f x) -- force was taken out due to BSBS, don't know it that's good
        etestf = testf f xs

newtonMethod :: Complex Double-> ComplexFunction -> ComplexFunction -> Int -> Double -> (Complex Double,Int)
newtonMethod z0 _ _ 0 _ = (z0,0)
newtonMethod z0 f f' ite threshold =
  let z =  (z0 - ((f z0) / (f' z0))) *(1:+0)
      delta = abs $ z - z0
  in
      if (realPart delta < threshold) && (imagPart delta < threshold) then (z,ite) else newtonMethod z f f' (ite - 1) threshold
