module Utilities (mapTuple,mapTuple3,
                  clamp,clampLoopedRGB,clampRGB,
                  scaleToComplex,scaleComplex,scaleVar)

where

import Data.Complex
import FractalSettings
import Data.List

mapTuple :: (Num a) => (a,a) -> (a -> a) -> (a,a)
mapTuple (a,b) f = (f a,f b)

mapTuple3 :: (Num a) => (a,a,a) -> (a -> b) -> (b,b,b)
mapTuple3 (a,b,c) f = (f a,f b,f c)

clamp :: (Ord a) => a -> (a,a) -> a
clamp x (maxX,minX) = newX
  where 
        newX | x > maxX = maxX
             | x < minX = minX
             | otherwise = x

scaleToComplex :: (Int,Int) -> (Double,Double) -> (Double,Double) -> 
                  (Double,Double) -> (Double,Double) -> Complex Double
scaleToComplex (x,y) (maxX,minX) (maxY,minY) 
                (scaledMaxX,scaledMinX) (scaledMaxY,scaledMinY) = (xnorm:+ynorm)
    where 
        xnorm = scaledMinX + 
            (((scaledMaxX - scaledMinX) * (fromIntegral x - minX)) / (maxX - minX))
        ynorm = scaledMinY + 
            (((scaledMaxY - scaledMinY) * (fromIntegral y - minY)) / (maxY - minY))

scaleComplex :: Complex Double -> (Double,Double) -> (Double,Double) -> 
                (Double,Double) -> (Double,Double) -> Complex Double
scaleComplex (x:+y) (mX,miX) (mY,miY) 
        (scaledMaxX,scaledMinX) (scaledMaxY,scaledMinY) = xnorm:+ynorm
    where 
        xnorm = scaledMinX + (((scaledMaxX - scaledMinX) * (x - miX)) / (mX - miX))
        ynorm = scaledMinY + (((scaledMaxY - scaledMinY) * (y - miY)) / (mY - miY))

scaleVar :: (Fractional a)=> a -> (a,a) -> (a,a) -> a
scaleVar x (maxX,minX) (newXMax,newXMin) = newXMin + (((newXMax - newXMin) * (cX - minX)) / (maxX - minX))
    where 
        cX  = x--clamp x (maxX,minX)

clampLoopedRGB ::(Num a, Integral a)=> (a,a,a) -> (ColorW8)
clampLoopedRGB (r,g,b) = mapTuple3 ((r,g,b)) (\x -> fromIntegral $ newX x)
    where 
        newX x = x `mod` 256

clampRGB ::(Num a, Integral a )=> (a,a,a) -> (ColorW8)
clampRGB (r,g,b) = mapTuple3 ((r,g,b)) (\x -> fromIntegral $ newX x)
    where 
        newX x = clamp x (255,0)
