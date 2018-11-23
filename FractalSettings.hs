module FractalSettings (FractalSettings(..),
                        AnimationType(..),
                        Parameters(..),
                        RenderSettings(..),
                        fsGenerate,
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
                        fsF,
                        fsF',
                        fsFs,
                        fsCreateParams,
                        fsCreateDim,
                        fsCreateBou,
                        fsCreateRenDist,
                        fsCreateRenCutoff,
                        getff',
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

--DistanceR Int     - colour based on distance to all roots
--                  takes shading Max Iterations
--Cutoff Int Double - colour based on root reached
--                  takes shading Max Iterations and cutoff point
--                  (compares root with actual root converged,
--                  if distance to actual root < cutoff point paint it black)
data RenderSettings = DistanceR Int
                    | Cutoff Int Double

-- list of colours and roots,
-- interpolates are functions which allow gradient sliding between colours for some animations,
-- then max Iterations and epsilon
data Parameters = Param RenderSettings RootCols [ColInterpolate] Int Double

-- Zoom (Compex Double) Double    - responsible for zoom animation
--                                  takes point to zoom to and speed of zoom
-- ParameterShift [ParameterModify] [Double]  - responsible for any animation
--                                  that is based around changing parameters
--                                  initial param comes from original fs,
--                                  double is the step per frame, can be negative
data AnimationType = Zoom (Complex Double) Double
                   | ParameterShift [ParameterModify] [Double]
                   | None

-- fsGenerate should be used to create Fractal settings
data FractalSettings = FS
    (ComplexFunction,ComplexFunction) -- function and its derivative
    ImageDimensions -- image dimensions to generate in pixels
    FractalBoundaries -- boundaries of fractal in complex doubles
    Parameters
    [AnimationType] -- how the fractal should be animated or [None]

colours = [(255,0,0),(0,255,255),(0,255,0),(255,165,0),(128,0,128),(255,255,0)] :: [ColorW8]
mandelbrotFunc z = (z*z*z) - (1:+0)
mandelbrotFunc' z = (z*z) * (3:+0)
mabr = (mandelbrotFunc,mandelbrotFunc')
mbRoots = [(1:+0),((-0.5):+sqrt(3)/2),((-0.5):+((-sqrt(3))/2))]
mbRC = zip mbRoots colours

cyclicFunc z= (900*(z^3)) - (2595*(z^2)) + (658*z) - 902
cyclicFunc' z= (2700*(z^2)) - ((5190*z)+658)
cycc = (cyclicFunc,cyclicFunc')
cRoots = [(11/4:+0),(1/15:+(-3/5)),(1/15:+3/5)]
cRC = zip cRoots colours

tworepFunc z = ((z-1)^2) *(z+1)
tworepFunc' z = (z-1)*((3*z)+1)
twre = (tworepFunc,tworepFunc')
twRoots = [((-1):+0),(1:+0),(1:+0)]
twRC = zip twRoots colours

fiverealFunc z = (z+2)*(z+1)*z*(z-1)*(z-2)
fiverealFunc' z = (5*(z^4))-(15*(z^2)) +4
fire = (fiverealFunc,fiverealFunc')
frRoots = [((-2):+0),((-1):+0),(0:+0),(1:+0),(2:+0)]
frRC = zip frRoots colours

fsCreate :: (ComplexFunction ,ComplexFunction) -> ImageDimensions -> FractalBoundaries -> RenderSettings -> RootCols -> Int -> Double -> [AnimationType] -> FractalSettings
fsCreate (f,f') imgDim fracBound renderSettings rootCols maxIters eps animType = FS (f,f') imgDim fracBound (fsCreateParams renderSettings rootCols maxIters eps) animType

fsGenerate :: Int -> ImageDimensions -> FractalBoundaries -> RenderSettings -> Int -> Double -> [AnimationType] -> FractalSettings
fsGenerate enum imgDim fracBound renderSettings iters eps animType =
    let
        (funcPair, rootColrs) = getff' enum
    in
        fsCreate funcPair imgDim fracBound renderSettings rootColrs iters eps animType

getff' :: Int -> ((ComplexFunction,ComplexFunction),RootCols)
getff' i | i == 0 = (mabr, mbRC)
         | i == 1 = (cycc, cRC)
         | i == 2 = (twre, twRC)
         | i == 3 = (fire, frRC)
         | otherwise = (mabr, mbRC)
fsCreateParams :: RenderSettings -> RootCols -> Int -> Double -> Parameters
fsCreateParams renderSettings rootCols maxIters eps = Param renderSettings rootCols (generateInterpolates rootCols) maxIters eps
fsCreateDim :: Int -> Int -> ImageDimensions
fsCreateDim x y = (x,y)
fsCreateBou :: Double -> Double -> Double -> Double -> (FractalBoundaries)
fsCreateBou x x2 y y2 = ((x,x2),(y,y2))
fsCreateRenCutoff :: Int -> Double -> RenderSettings
fsCreateRenCutoff shadingTop cutoff = Cutoff shadingTop cutoff
fsCreateRenDist :: Int -> RenderSettings
fsCreateRenDist shadingTop = DistanceR shadingTop
fsDim :: FractalSettings -> ImageDimensions
fsDim (FS _ (d) _ _ _) = d
fsHei :: FractalSettings -> Int
fsHei (FS _ (h,_) _ _ _) = h
fsWid :: FractalSettings -> Int
fsWid (FS _ (_,w) _ _ _) = w
fsF :: FractalSettings -> ComplexFunction
fsF (FS f _ _ _ _) = fst f
fsF' :: FractalSettings -> ComplexFunction
fsF' (FS f' _ _ _ _) = snd f'
fsFs :: FractalSettings -> (ComplexFunction,ComplexFunction)
fsFs (FS f _ _ _ _) = f
fsBound :: FractalSettings -> FractalBoundaries
fsBound (FS _ _ b _ _) = b
fsXBound :: FractalSettings -> XYDouble
fsXBound (FS _ _ ((maxX,minX),_) _ _) = (maxX,minX)
fsYBound :: FractalSettings -> XYDouble
fsYBound (FS _ _  (_,(maxY,minY)) _ _) = (maxY,minY)
fsXBound1 :: FractalSettings -> Double
fsXBound1 (FS _ _ ((maxX,minX),_) _ _) = maxX
fsXBound2 :: FractalSettings -> Double
fsXBound2 (FS _ _ ((maxX,minX),_) _ _) =  minX
fsYBound1 :: FractalSettings -> Double
fsYBound1 (FS _ _ (_,(maxY,minY)) _ _) = maxY
fsYBound2 :: FractalSettings -> Double
fsYBound2 (FS _ _ (_,(maxY,minY)) _ _) =  minY
fsRendS :: FractalSettings -> RenderSettings
fsRendS (FS _ _ _ (Param renderSettings _ _ _ _) _) = renderSettings
fsRootCols :: FractalSettings -> RootCols
fsRootCols (FS _ _ _ (Param _ rootcols _ _ _) _) = rootcols
fsIters :: FractalSettings -> Int
fsIters (FS _ _ _ (Param _ _ _ maxIters _) _) = maxIters
fsEpsilon :: FractalSettings -> Double
fsEpsilon (FS _ _ _ (Param _ _ _ _ eps) _) = eps
fsAnimType :: FractalSettings -> [AnimationType]
fsAnimType (FS _ _ _ _ a) = a
fsParams :: FractalSettings -> Parameters
fsParams (FS _ _ _ p _) = p

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
