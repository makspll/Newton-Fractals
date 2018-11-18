
module NewtonianMethod ( newtonMethod, mapFractal, generateImage2)

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
generateImage :: ComplexFunction -> ComplexFunction -> (Double,Double) -> (Double,Double) -> (Int,Int) -> Int -> Double -> [[(Complex Double, Int)]]
generateImage f f'  (blah,blah2) (fWidth,fHeight) (width,height) iters eps =concat.concat $ [[mapFractal (f) (f') ((fst gridImagDim) * (toD nX),(snd gridImagDim)* (toD nY)) (fractalDAtN (fst gridImagDim) nX,fractalDAtN (snd gridImagDim) nY) (realDatN (fst gridRealDim) nX,realDatN (snd gridRealDim) nY) iters eps | nX <- [0..(noGridsX-1)] ]|nY <- [0..(noGridsY-1)]]
    where gridRealDim = ((width `div`noGridsX),(height `div` noGridsY))
          gridImagDim = ((fWidth / (toD noGridsX)),(fHeight / (toD noGridsY)))
          fractalDAtN oneD n = oneD + (oneD *(toD n))
          realDatN oneD n = oneD + (oneD * n)
          toD = fromIntegral
          (noGridsX,noGridsY) = (100,1) :: (Int,Int)
generateImage2 :: ComplexFunction -> ComplexFunction -> (Double,Double) -> (Double,Double) -> (Int,Int) -> Int -> Double -> [[(Complex Double, Int)]]
generateImage2 f f' (minfX,minfY) (fTotalW,fTotalH) (pixTotalW,pixTotalH) inters eps = concat [rowN (toD n)|n <- [0..(pixTotalH-1)]]
  where rowN n = mapFractal f f' (minfX,fTotalH - (n * (fTotalH/ (toD pixTotalH)))) (fTotalW,(fTotalH/ (toD pixTotalH))) (pixTotalW,1) inters eps
        toD = fromIntegral
newtonMethod :: Complex Double-> ComplexFunction -> ComplexFunction -> Int -> Double -> (Complex Double,Int)
newtonMethod z0 _ _ 0 _ = (z0,0)
newtonMethod z0 f f' ite threshold =
  let z =  (z0 - ((f z0) / (f' z0))) *(1:+0)
      delta = abs $ z - z0
  in
      if (realPart delta < threshold) && (imagPart delta < threshold) then (z,ite) else newtonMethod z f f' (ite - 1) threshold
