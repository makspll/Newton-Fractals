module FractalSettings (FractalSettings(..),
                        AnimationType(..),
                        Parameters(..),
                        RenderSettings(..),
                        fsCreate,
                        fsDim,
                        fsHei,
                        fsWid,
                        fsBound,
                        fsXBound,
                        fsYBound,
                        fsXBound1,
                        fsXBound2,
                        fsYBound1,
                        fsYBound2,
                        fsRendS,
                        fsRootCols,
                        fsIters,
                        fsEpsilon,
                        fsAnimType,
                        fsParams,
                        XYInt,XYDouble,ColorW8,Pixel,RootCols,ComplexFunction,FractalBoundaries,ParameterModify,ImageDimensions)
where

import Data.Complex
import Data.Word
import Data.List (delete)
type XYInt = (Int,Int)
type XYDouble = (Double,Double)
type ColorW8  = (Word8,Word8,Word8)
type Pixel = (ColorW8)
type RootCol =(Complex Double,ColorW8)
type RootCols = [(Complex Double,ColorW8)]
type ComplexFunction = (Complex Double -> Complex Double)
type FractalBoundaries = (XYDouble,XYDouble) -- (Xmax,Xmin) (Ymax,Ymin)
type ColInterpolate = Int -> Int
type ParameterModify =  Parameters -> Double -> Parameters
type ImageDimensions = (XYInt) -- (width,height)
data RenderSettings = DistanceR Int |  --colour based on distance to all roots - takes shading Max Iterations
                      Cutoff Int Double  -- colour based on root reached - takes shading Max Iterations and cutoff point (compares root with actual root converged, if distance to actual root < cutoff point paint it black)
data Parameters = Param RenderSettings RootCols [ColInterpolate] Int Double-- list of colours and roots,interpolates are functions which allow gradient sliding between colours for some animations, then max Iterations and epsilon
-- colour Mode, Iterations, shading upper iterations, epsilon, rootColor cutoff
data AnimationType = Zoom (Complex Double) Double      |
                     ParameterShift [ParameterModify] [Double]   | -- initial param comes from original fs, double is the step per frame, can be negative
                     None
data FractalSettings = FS ImageDimensions FractalBoundaries Parameters [AnimationType]

fsCreate :: ImageDimensions -> FractalBoundaries -> RenderSettings -> RootCols -> Int -> Double -> [AnimationType] -> FractalSettings
fsCreate imgDim fracBound renderSettings rootCols maxIters eps animType = FS imgDim fracBound (fsCreateParams renderSettings rootCols maxIters eps) animType
fsCreateParams :: RenderSettings -> RootCols -> Int -> Double -> Parameters
fsCreateParams renderSettings rootCols maxIters eps = Param renderSettings rootCols (generateInterpolates rootCols) maxIters eps

fsDim :: FractalSettings -> ImageDimensions
fsDim (FS (d) _ _ _) = d
fsHei :: FractalSettings -> Int
fsHei (FS (h,_) _ _ _) = h
fsWid :: FractalSettings -> Int
fsWid (FS (_,w) _ _ _) = w
fsBound :: FractalSettings -> FractalBoundaries
fsBound (FS _ b _ _) = b
fsXBound :: FractalSettings -> XYDouble
fsXBound (FS _ ((maxX,minX),_) _ _) = (maxX,minX)
fsYBound :: FractalSettings -> XYDouble
fsYBound (FS _ (_,(maxY,minY)) _ _) = (maxY,minY)
fsXBound1 :: FractalSettings -> Double
fsXBound1 (FS _ ((maxX,minX),_) _ _) = maxX
fsXBound2 :: FractalSettings -> Double
fsXBound2 (FS _ ((maxX,minX),_) _ _) =  minX
fsYBound1 :: FractalSettings -> Double
fsYBound1 (FS _ (_,(maxY,minY)) _ _) = maxY
fsYBound2 :: FractalSettings -> Double
fsYBound2 (FS _ (_,(maxY,minY)) _ _) =  minY
fsRendS :: FractalSettings -> RenderSettings
fsRendS (FS _ _ (Param renderSettings _ _ _ _) _) = renderSettings
fsRootCols :: FractalSettings -> RootCols
fsRootCols (FS _ _ (Param _ rootcols _ _ _) _) = rootcols
fsIters :: FractalSettings -> Int
fsIters (FS _ _ (Param _ _ _ maxIters _) _) = maxIters
fsEpsilon :: FractalSettings -> Double
fsEpsilon (FS _ _ (Param _ _ _ _ eps) _) = eps
fsAnimType :: FractalSettings -> [AnimationType]
fsAnimType (FS _ _ _ a) = a
fsParams :: FractalSettings -> Parameters
fsParams (FS _ _ p _) = p


generateInterpolates :: RootCols -> [(Int -> Int)]
generateInterpolates rootCols = [redInterpolate,blueInterpolate,greenInterpolate]
  where redInterpolate = generateInterpolate $ foldr (\(rt,(r,g,b)) acc -> (fromIntegral r) : acc ) [] rootCols
        blueInterpolate = generateInterpolate $ foldr (\(rt,(r,g,b)) acc -> (fromIntegral b) : acc ) [] rootCols
        greenInterpolate = generateInterpolate $ foldr (\(rt,(r,g,b)) acc -> (fromIntegral g) : acc ) [] rootCols

generateInterpolate :: [Int] -> (Int -> Int)
generateInterpolate cols = interpolateCChannel cols

interpolateCChannel :: [Int] -> Int -> Int
interpolateCChannel cols index = round (lagrange index)
    where xys = zip xPosS cols
          distance =  255 / (fromIntegral ( length cols)) :: Float
          xPosS= map (\x -> (distance * (fromIntegral x)) ) [0..(length cols -1)]
          lagrange x = let
                  lamb xi  = product $ map (\xj -> ((fromIntegral x)-xj) / (xi-xj)) (delete xi xPosS)
                  in sum $ zipWith (\x y -> (fromIntegral x) * y) cols (map lamb xPosS)
