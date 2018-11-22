module Rendering (write,simAnimate,psIterations,psEpsilon,psRootCols,generateImage)

where

import Utilities
import FractalSettings
import Codec.BMP
import Data.Complex
import NewtonianMethod
import qualified Data.ByteString as BS

filename = "fractal"
write ::  FractalSettings -> String -> IO ()
write fs filename =  writeBMP filename bmp >> putStrLn ("Saved:" ++ filename)
  where rgba = generateImage fs
        bmp = packRGBA32ToBMP (fsWid fs) (fsHei fs) (rgba)

simAnimate :: FractalSettings -> Int -> IO ()
simAnimate fs frame = write nextfs (filename ++ "-" ++ (show frame) ++ ".bmp")
    where     f = fsF fs
              f' = fsF' fs
              nextfs = foldr(\animI newfs ->case animI of
                                          rs@(Zoom c z) -> zoomToRoot c z newfs frame
                                          (ParameterShift funcs steps) -> applyParameterShift funcs steps newfs frame
                                          (None)       -> newfs                                  ) fs (fsAnimType fs)


generateImageWithFrame :: FractalSettings -> Int -> BS.ByteString
generateImageWithFrame fs frame = generateImage nextfs
    where     f = fsF fs
              f' = fsF' fs
              nextfs = foldr(\animI newfs ->case animI of
                                          rs@(Zoom c z) -> zoomToRoot c z newfs frame
                                          (ParameterShift funcs steps) -> applyParameterShift funcs steps newfs frame
                                          (None)       -> newfs                                  ) fs (fsAnimType fs)

zoomToRoot :: Complex Double -> Double -> FractalSettings -> Int -> FractalSettings
zoomToRoot _ _ fs 0 = fs
zoomToRoot (a:+b) zoomFactor fs frame = FS (fsFs fs) (fsDim fs) ((a + deltaX,a - deltaX),(b + deltaY, b - deltaY)) (fsParams fs) (fsAnimType fs)
  where curDim = (abs (fsXBound1 fs - fsXBound2 fs),abs (fsYBound1 fs - fsYBound2 fs))
        newDimension = mapTuple (curDim) (*(1/(zoomFactor * fromIntegral frame))) --frames start at 1
        deltaX = (fst newDimension) /2
        deltaY = (snd newDimension) /2

applyParameterShift :: [ParameterModify] -> [Double] -> FractalSettings -> Int -> FractalSettings
applyParameterShift funcs steps fs frame = newfs
  where
    newfs = do
            let fsSteps = zip funcs steps
            (FS (fsFs fs) (fsDim fs) (fsBound fs) ( foldr (\(f,s) params -> f params (s*(fromIntegral frame))) (fsParams fs) fsSteps ) (fsAnimType fs)) -- only thing that is different each run is frame count, fs always is the original fractal setting
---Parameter Shifters
psIterations :: ParameterModify
psIterations (Param constRenderSettings constRootCols constInterpolates constIters constEpsilon) currentDelta = newParam
    where newParam = Param constRenderSettings constRootCols constInterpolates (floor $ currentDelta + (fromIntegral constIters)) constEpsilon

psEpsilon :: ParameterModify
psEpsilon (Param constRenderSettings constRootCols constInterpolates constIters constEpsilon) currentDelta = newParam
    where newParam = Param constRenderSettings constRootCols constInterpolates constIters (currentDelta + constEpsilon)

psRootCols :: ParameterModify
psRootCols (Param constRenderSettings constRootCols constInterpolates constIters constEpsilon) currentDelta = newParam
    where newParam = Param constRenderSettings (newRootCols) constInterpolates constIters constEpsilon
          newRootCols = zipWith (\x y -> x y) test [0..(length constRootCols -1)]
          test = map (\x -> (newRootCol x)) constRootCols :: [Int -> (Complex Double,ColorW8)]
          newRootCol (rt,(r,g,b)) i = (rt,(clampRGB (redInterpolate $ interpolXAtChannel i,greenInterpolate $ interpolXAtChannel i,blueInterpolate $ interpolXAtChannel i))) :: (Complex Double,ColorW8) -- interpolXAtChannel g,  interpolXAtChannel  b
          interpolXAtChannel i = ((round(currentDelta)) + ((i * (255 `div`(length constRootCols)))))`mod`256 :: Int
          redInterpolate = constInterpolates !! 0
          blueInterpolate = constInterpolates !! 1
          greenInterpolate = constInterpolates !! 2
