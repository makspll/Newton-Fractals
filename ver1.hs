
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


colours = [(255,0,0),(0,255,255),(0,255,0),(255,165,0),(128,0,128),(255,255,0)] :: [ColorW8]
defaultWindowSize = (500,500)
defaultSlice = ((10,-10),(10,-10))
filename = "fractal"

-------------------------------------------------------------------------Functions
coolAsFuckColours = [(255,0,0),(0,255,0),(0,255,0)]

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
--f maxIter threshold (width,height) fracMaxMinX fracMaxMinY colours

-----------------------------------------------------------------------------User Input
{-
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
                                             return $ fsCreate mandelbrotFunc mandelbrotFunc' (w,h) ((mxX,mnX),(mxY,mnY))  (Cutoff colIte colE) mbRC iter eps [(Zoom (x:+y) zf)]
                                    (_) -> return $ fsCreate mandelbrotFunc mandelbrotFunc' (w,h) ((mxX,mnX),(mxY,mnY)) (DistanceR colIte) mbRC iter eps [(Zoom (x:+y) zf)]
          fracSettings

validateF :: String -> Double
validateF x = fromMaybe (0.0) $readMaybe (x)

validateI :: String -> Int
validateI x = fromMaybe (0) $readMaybe (x)
-}
-----------------------------------------------------------------------------Rendering
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

---------------------------------------------------------------------------- Impure Part
--doAnimate :: Int -> Complex Double -> Double -> IO ()
--doAnimate n (a:+b) z = mapM_ (animateF (a:+b) z) [1..n]
testSettings2 = fsCreate fire defaultWindowSize defaultSlice (Cutoff 20 0.1) frRC 20 0.1 [(ParameterShift [psRootCols] [1]),(Zoom (0:+0) 2)]
#ifdef __GUI_APP

main = do
    win <- Win.create $ generateImageWithFrame
    return (0)

#else

main = do
       getNumCapabilities >>= setNumCapabilities
       putStrLn "Test Mode, generating Test Fractal"
       --nm <- getLine
       --fs <- inputFS
      -- let n = validateI nm
       let n = 50
       let fs = testSettings2
       if n <= 1 then
         write fs (filename ++ ".bmp")
       else
         mapM_ (simAnimate fs ) [0..n]
       --(simAnimate testSettings (Zoom (0:+0) 2) (mandelbrotFunc) (mandelbrotFunc')) [0..10]
#endif
