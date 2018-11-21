
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
filename = "fractal"

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
                                             return $ fsCreate (w,h) ((mxX,mnX),(mxY,mnY))  (Cutoff colIte colE) rootcolours iter eps [(Zoom (x:+y) zf)]
                                    (_) -> return $ fsCreate (w,h) ((mxX,mnX),(mxY,mnY)) (DistanceR colIte) rootcolours iter eps [(Zoom (x:+y) zf)]
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

simAnimate :: ComplexFunction -> ComplexFunction -> FractalSettings -> Int -> IO ()
simAnimate f f' fs frame = write f f' nextfs (filename ++ "-" ++ (show frame) ++ ".bmp")
    where nextfs = foldr(\animI newfs ->case animI of
                                          rs@(Zoom c z) -> zoomToRoot c z newfs frame
                                          (ParameterShift funcs steps) -> applyParameterShift funcs steps newfs frame
                                          (None)       -> newfs                                  ) fs (fsAnimType fs)


zoomToRoot :: Complex Double -> Double -> FractalSettings -> Int -> FractalSettings
zoomToRoot _ _ fs 0 = fs
zoomToRoot (a:+b) zoomFactor fs frame = FS (fsDim fs) ((a + deltaX,a - deltaX),(b + deltaY, b - deltaY)) (fsParams fs) (fsAnimType fs)
  where curDim = (abs (fsXBound1 fs - fsXBound2 fs),abs (fsYBound1 fs - fsYBound2 fs))
        newDimension = mapTuple (curDim) (*(1/(zoomFactor * fromIntegral frame))) --frames start at 1
        deltaX = (fst newDimension) /2
        deltaY = (snd newDimension) /2

applyParameterShift :: [ParameterModify] -> [Double] -> FractalSettings -> Int -> FractalSettings
applyParameterShift funcs steps fs frame = newfs
  where
    newfs = do
            let fsSteps = zip funcs steps
            (FS (fsDim fs) (fsBound fs) ( foldr (\(f,s) params -> f params (s*(fromIntegral frame))) (fsParams fs) fsSteps ) (fsAnimType fs)) -- only thing that is different each run is frame count, fs always is the original fractal setting
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

---------------------------------------------------------------------------- Impure Part
doAnimate :: Int -> Complex Double -> Double -> IO ()
doAnimate n (a:+b) z = mapM_ (animateF (a:+b) z) [1..n]
testSettings2 = fsCreate (500,500) ((1,-1),(1,-1)) (Cutoff 20 0.01) rootcolours 20 0.000001 [(ParameterShift [psIterations] [1,1]),(Zoom (0:+0) 2.2)]
#ifdef __GUI_APP

main = do
    win <- Win.create $ generateImage mandelbrotFunc mandelbrotFunc'
    return (0)

#else

main = do
       getNumCapabilities >>= setNumCapabilities
       putStrLn "Test Mode, generating Test Fractal"
       --nm <- getLine
       --fs <- inputFS
      -- let n = validateI nm
       let n = 340
       let fs = testSettings2
       if n <= 1 then
         write (mandelbrotFunc) (mandelbrotFunc') fs (filename ++ ".bmp")
       else
         mapM_ (simAnimate (mandelbrotFunc) (mandelbrotFunc') fs ) [0..n]
       --(simAnimate testSettings (Zoom (0:+0) 2) (mandelbrotFunc) (mandelbrotFunc')) [0..10]
#endif
