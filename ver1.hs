
import Data.Complex
import qualified Data.ByteString as BS
import Data.Word
import Codec.BMP
import Control.Concurrent
import Text.Read
import Data.Maybe

import NewtonianMethod

#ifdef __GUI_APP
import Window as Win
#endif

type XYInt = (Int,Int)
type XYDouble = (Double,Double)
type ColorW8  = (Word8,Word8,Word8)
type Pixel = (ColorW8)
type RootCols = [(Complex Double,ColorW8)]
type ComplexFunction = (Complex Double -> Complex Double)
type FractalBoundaries = (XYDouble,XYDouble) -- (Xmax,Xmin) (Ymax,Ymin)
type ParameterModify = Parameters -> XYDouble -> Int -> Parameters
type ImageDimensions = (XYInt) -- (width,height)
data RenderSettings = DistanceR Int |  --colour based on distance to all roots - takes shading Max Iterations
                      Cutoff Int Double  -- colour based on root reached - takes shading Max Iterations and cutoff point (compares root with actual root converged, if distance to actual root < cutoff point paint it black)
data Parameters = Param RenderSettings RootCols Int Double -- list of colours and roots then max Iterations and epsilon
-- colour Mode, Iterations, shading upper iterations, epsilon, rootColor cutoff
data AnimationType = Zoom (Complex Double) Double      |
                     ParameterShift ParameterModify XYDouble Int   | --
                     None
data FractalSettings = FS ImageDimensions FractalBoundaries Parameters AnimationType

colours = [(255,0,0),(0,255,255),(0,255,0)]
roots = [(1:+0),((-0.5):+sqrt(3)/2),((-0.5):+((-sqrt(3))/2))]
rootcolours = zip roots colours
testSettings = FS (2000,2000) ((1,-1),(1,-1)) (Param (DistanceR 20) rootcolours 20 0.000001)
filename = "fractal"

fractalMaxMinX = (1,-1)
fractalMaxMinY = (1,-1)
imageSize = (2000,2000)
iterations = 30 :: Int
shadingMaxIter = 30
epsilon = 0.000001
rootColorThreshold = 0.000001
colourMode = "DistanceR"
fsCreate :: ImageDimensions -> FractalBoundaries -> Parameters -> AnimationType -> FractalSettings
fsCreate imgDim fracBound params animType = FS imgDim fracBound params animType
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
fsRendS (FS _ _ (Param renderSettings _ _ _) _) = renderSettings
fsRootCols :: FractalSettings -> RootCols
fsRootCols (FS _ _ (Param _ rootcols _ _) _) = rootcols
fsIters :: FractalSettings -> Int
fsIters (FS _ _ (Param _ _ maxIters _) _) = maxIters
fsEpsilon :: FractalSettings -> Double
fsEpsilon (FS _ _ (Param _ _ _ eps) _) = eps
fsAnimType :: FractalSettings -> AnimationType
fsAnimType (FS _ _ _ a) = a
fsParams :: FractalSettings -> Parameters
fsParams (FS _ _ p _) = p
-------------------------------------------------------------------------Generator

mandelbrotFunc z = (z*z*z) - (1:+0)
mandelbrotFunc' z = (z*z) * (3:+0)

--f maxIter threshold (width,height) fracMaxMinX fracMaxMinY colours

applyColourFunc :: FractalSettings -> (Complex Double,Int) -> [Word8]
applyColourFunc fs (root,ite) = case (fsRendS fs) of
                                    (Cutoff i d)   -> colourWithCutOff (fsRootCols fs) (root,ite) d i
                                    (DistanceR i)  -> colourWithDToR (fsRootCols fs) (root,ite) i
    where the [] = error "noColours found for"
          the [x] = x

colourWithCutOff :: RootCols -> (Complex Double, Int) -> Double -> Int -> [Word8]
colourWithCutOff _ (_,0) _ _  = [0,0,0,255]
colourWithCutOff rootColours (root,ite) cutoff maxShadeIter  = [r,g,b,255]
  where scaledIte = scaleVar (fromIntegral ite) (fromIntegral maxShadeIter,0) (1,0)
        filteredByEpsilon = filter (\(r,c) -> abs(realPart (r - root)) < cutoff && abs(imagPart (r - root)) < cutoff) rootColours
        (r,g,b) = mapTuple3 (snd $ the filteredByEpsilon)  (\x -> round $(fromIntegral x)* scaledIte) --bound this
        the [] = ((0:+0),(0,0,0)) --If not converged but not to any root, paint it black
        the [x] = x
        the ((r,c):xs) = (r,(foldr (\(r,c) c2-> mixRGB c c2) c (xs))) -- if within range of multiple roots mix colours

colourWithDToR :: RootCols -> (Complex Double, Int) -> Int -> [Word8]
colourWithDToR _ (_,0) _ = [0,0,0,255]
colourWithDToR rootColours (root,ite) maxShadeIter = [r,g,b,255]
  where scaledIte = scaleVar (fromIntegral ite) (fromIntegral maxShadeIter,0) (1,0)
        switchToDist = map (\(r,c) -> (((distanceFactor r root):+0),(c))) rootColours :: [((Complex Double),(ColorW8))]
        distanceFactor rt1 rt2 = (scaleVar (magnitude ((rt1) - rt2)) (100,0) (0,1))
        (r,g,b) = foldr1 (\c c2-> mixRGB c c2) [mapTuple3 (cr,cg,cb) (\x-> round $(fromIntegral x)*df *scaledIte)| ((df:+nan),(cr,cg,cb))<-switchToDist] --bound this

-----------------------------------------------------------------------------User Input
inputFS :: IO FractalSettings --testSettings = FS (2000,2000) ((1,-1),(1,-1)) (Param (DistanceR 30) rootcolours 20 0.000001)
inputFS =do
          putStrLn "Image Width"
          wm <- getLine
          let w = validateI wm
          putStrLn "Image Height"
          hm <- getLine
          let h = validateI hm
          putStrLn "Fractal Upper X"
          mxXm <- getLine
          let mxX = validateF mxXm
          putStrLn "Fractal Lower X"
          mnXm <- getLine
          let mnX = validateF mnXm
          putStrLn "Fractal Upper Y"
          mxYm <- getLine
          let mxY = validateF mxYm
          putStrLn "Fractal Lower Y"
          mnYm <- getLine
          let mnY = validateF mnYm
          putStrLn "Colour Mode Cutoff or Distance (1 or 2)"
          mode <- getLine
          putStrLn "Colour Iterations Upper Shader"
          colItem <- getLine
          let colIte = validateI colItem
          putStrLn "Max iterations"
          iterm <- getLine
          let iter = validateI iterm
          putStrLn "Epsilon"
          epsm <- getLine
          let eps = validateF epsm
          putStrLn "x coordinate of zoom"
          xm <- getLine
          let x = validateF xm
          putStrLn "y coordinate of zoom"
          ym <- getLine
          let y = validateF ym
          putStrLn "Zoom per frame (>1 for zoom in)"
          zfm <- getLine
          let zf = validateF zfm
          let fracSettings = case mode of
                                    ("1") -> do
                                             putStrLn "Colour Cutoff Threshold (= epsilon for normal fractal)"
                                             colEm <- getLine
                                             let colE = validateF colEm
                                             return $ fsCreate (w,h) ((mxX,mnX),(mxY,mnY)) (Param (Cutoff colIte colE) rootcolours iter eps) (Zoom (x:+y) zf)
                                    (_) -> return $ fsCreate (w,h) ((mxX,mnX),(mxY,mnY)) (Param (DistanceR colIte) rootcolours iter eps) (Zoom (x:+y) zf)
          fracSettings

validateF :: String -> Double
validateF x = fromMaybe (0.0) $readMaybe (x)

validateI :: String -> Int
validateI x = fromMaybe (0) $readMaybe (x)

-----------------------------------------------------------------------------Rendering
write :: ComplexFunction -> ComplexFunction -> FractalSettings -> String -> IO ()
write f f' fs filename =  writeBMP filename bmp >> putStrLn ("Saved:" ++ filename)
  where rgba = map (map (applyColourFunc fs)) 
            (mapFractal f f' (fsXBound2 fs, fsYBound2 fs) (fsXBound1 fs - fsXBound2 fs, fsYBound1 fs - fsYBound2 fs) (fsWid fs, fsHei fs) (fsIters fs) (fsEpsilon fs) )
        bmp = packRGBA32ToBMP (fsWid fs) (fsHei fs) (BS.pack $ concat.concat $ rgba)

simAnimate :: FractalSettings -> ComplexFunction -> ComplexFunction -> Int -> IO ()
simAnimate fs f f' frame = write f f' nextfs (filename ++ "-" ++ (show frame) ++ ".bmp")
    where nextfs = case fsAnimType fs of
                    rs@(Zoom c z) -> zoomToRoot c z fs frame
                    (ParameterShift f ud s) -> applyParameterShift fs frame
                    (None)       -> fs


zoomToRoot :: Complex Double -> Double -> FractalSettings -> Int -> FractalSettings
zoomToRoot _ _ fs 0 = fs
zoomToRoot (a:+b) zoomFactor fs frame = fsCreate (fsDim fs) ((a + deltaX,a - deltaX),(b + deltaY, b - deltaY)) (fsParams fs) (fsAnimType fs)
  where curDim = (abs (fsXBound1 fs - fsXBound2 fs),abs (fsYBound1 fs - fsYBound2 fs))
        newDimension = mapTuple (curDim) (*(1/(zoomFactor * fromIntegral frame))) --frames start at 1
        deltaX = (fst newDimension) /2
        deltaY = (snd newDimension) /2
applyParameterShift :: FractalSettings -> Int -> FractalSettings
applyParameterShift fs frame = newfs
  where
    newfs = do
            let (ParameterShift f (up,down) step) = fsAnimType fs
            (fsCreate (fsDim fs) (fsBound fs) (f (fsParams fs) (up,down) (step*frame)) (fsAnimType fs))
---Parameter Shifters
psIterations :: ParameterModify
psIterations (Param renderSettings rootCols iters epsilon) (up,down) step = newParam
    where newParam = Param renderSettings rootCols (step + (round down)) epsilon
----------------------------------------------------------------------------- Utility
mapTuple :: (Num a) => (a,a) -> (a -> a) -> (a,a)
mapTuple (a,b) f = (f a,f b)

mapTuple3 :: (Num a) => (a,a,a) -> (a -> a) -> (a,a,a)
mapTuple3 (a,b,c) f = (f a,f b,f c)

clamp :: (Ord a) => a -> (a,a) -> a
clamp x (maxX,minX) = newX
  where newX| x > maxX = maxX
            | x < minX = minX
            | otherwise = x
mixRGB :: ColorW8 -> ColorW8 -> ColorW8
mixRGB (c1,c2,c3) (a1,a2,a3) = mapTuple3 ((c1r+a1r),(c2r+a2r),(c3r+a3r)) (\x-> clamp x (255,0) )
  where c1r = fromIntegral c1
        c2r = fromIntegral c2
        c3r = fromIntegral c3
        a1r = fromIntegral a1
        a2r = fromIntegral a2
        a3r = fromIntegral a3

scaleToComplex :: (Int,Int) ->(Double,Double)->(Double,Double)-> (Double,Double) -> (Double,Double) -> Complex Double
scaleToComplex (x,y) (maxX,minX) (maxY,minY) (scaledMaxX,scaledMinX) (scaledMaxY,scaledMinY) = (xnorm:+ynorm)
  where xnorm = scaledMinX + (((scaledMaxX - scaledMinX) * (fromIntegral x - minX)) / (maxX - minX))
        ynorm = scaledMinY + (((scaledMaxY - scaledMinY) * (fromIntegral y - minY)) / (maxY - minY))

scaleComplex :: Complex Double-> (Double,Double)->(Double,Double)-> (Double,Double) -> (Double,Double) -> Complex Double
scaleComplex (x:+y) (mX,miX) (mY,miY) (scaledMaxX,scaledMinX) (scaledMaxY,scaledMinY) = xnorm:+ynorm
  where xnorm = scaledMinX + (((scaledMaxX - scaledMinX) * (x - miX)) / (mX - miX))
        ynorm = scaledMinY + (((scaledMaxY - scaledMinY) * (y - miY)) / (mY - miY))

scaleVar :: (Fractional a)=> a -> (a,a) -> (a,a) -> a
scaleVar x (maxX,minX) (newXMax,newXMin) = newXMin + (((newXMax - newXMin) * (cX - minX)) / (maxX - minX))
    where cX  = x--clamp x (maxX,minX)

---------------------------------------------------------------------------- Impure Part
--doAnimate :: Int -> Complex Double -> Double -> IO ()
--doAnimate n (a:+b) z = mapM_ (animateF (a:+b) z) [1..n]
testSettings2 = FS (100,100) ((1,-1),(1,-1)) (Param (Cutoff 20 0.000001) rootcolours 20 0.000001) (ParameterShift (psIterations) (0,1) 3)

#ifdef __GUI_APP
main = do
    win <- Win.create
    return (0)

#else
main = do
       Control.Concurrent.setNumCapabilities 6
       putStrLn "Frames number:"
       nm <- getLine
       --fs <- inputFS
       let n = validateI nm
       let fs = testSettings2
       if n <= 1 then
         write (mandelbrotFunc) (mandelbrotFunc') fs (filename ++ ".bmp")
       else
         mapM_ (simAnimate fs (mandelbrotFunc) (mandelbrotFunc')) [0..n]
       --(simAnimate testSettings (Zoom (0:+0) 2) (mandelbrotFunc) (mandelbrotFunc')) [0..10]
#endif
