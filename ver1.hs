
import Data.Complex
import qualified Data.ByteString as BS
import Data.Word
import Codec.BMP
import Control.Concurrent
import Text.Read
import Data.Maybe
import Utilities
import NewtonianMethod
import FractalSettings

#ifdef __GUI_APP
import Window as Win
#endif


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

-------------------------------------------------------------------------Generator

mandelbrotFunc z = (z*z*z) - (1:+0)
mandelbrotFunc' z = (z*z) * (3:+0)

--f maxIter threshold (width,height) fracMaxMinX fracMaxMinY colours

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
  where rgba = generateImage f f' fs
        bmp = packRGBA32ToBMP (fsWid fs) (fsHei fs) (rgba)

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
---------------------------------------------------------------------------- Impure Part
--doAnimate :: Int -> Complex Double -> Double -> IO ()
--doAnimate n (a:+b) z = mapM_ (animateF (a:+b) z) [1..n]
testSettings2 = FS (2000,2000) ((1,-1),(1,-1)) (Param (Cutoff 20 0.000001) rootcolours 20 0.000001) (ParameterShift (psIterations) (0,1) 3)

#ifdef __GUI_APP
main = do
    win <- Win.create $ generateImage mandelbrotFunc mandelbrotFunc'
    return (0)

#else
main = do
       getNumCapabilities >>= setNumCapabilities
       putStrLn "Frames number:"
       nm <- getLine
       --fs <- inputFS
       let n = validateI nm
       let fs = testSettings2
       if n <= 1 then
         write (mandelbrotFunc) (mandelbrotFunc') fs (filename ++ ".bmp")
       else
         mapM_ (simAnimate fs (mandelbrotFunc) (mandelbrotFunc')) [0..n]
       main
       --(simAnimate testSettings (Zoom (0:+0) 2) (mandelbrotFunc) (mandelbrotFunc')) [0..10]
#endif
