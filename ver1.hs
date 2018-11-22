
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
import qualified Window as Win
#endif


defaultWindowSize = (500,500)
defaultSlice = ((10,-10),(10,-10))
filename = "fractal"

-----------------------------------------------------------------------------User Input

inputFS :: IO FractalSettings --testSettings = FS (2000,2000) ((1,-1),(1,-1)) (Param (DistanceR 30) rootcolours 20 0.000001)
inputFS = undefined
--FS (ComplexFunction,ComplexFunction) ImageDimensions FractalBoundaries Parameters [AnimationType]
inputDefaultFS :: IO (ComplexFunction,ComplexFunction)
inputDefaultFS = undefined

validateF :: String -> Double
validateF x = fromMaybe (0.0) $readMaybe (x)

validateI :: String -> Int
validateI x = fromMaybe (0) $readMaybe (x)
getFS :: IO (FractalSettings)
getFS =do
       imgDims<- getImageDims
       fracBound <- getBoundaries
       (params,ff')<- getParametersPlusFf
       animTypes <- getAnims
       return $ FS ff' imgDims fracBound params animTypes
getImageDims :: IO (ImageDimensions)
getImageDims = do
               putStrLn "--- --- BMP Dimensions --- ---"
               wid <- getVar "Enter Image Width: "
               let widthV = validateI wid
               hei <- getVar "Enter Image Height: "
               let heightV = validateI hei
               return $ fsCreateDim (widthV) (heightV)
getBoundaries :: IO (FractalBoundaries)
getBoundaries = do
                putStrLn "--- --- Fractal Boundaries --- ---"
                xm <- getVar "Enter Left X Coordinate: "
                let xmV = validateF xm
                xmx <- getVar "Enter Right X Coordinate: "
                let xmxV = validateF xmx
                ym <- getVar "Enter Bottom Y Coordinate: "
                let ymV = validateF ym
                ymx <- getVar "Enter Top Y Coordinate: "
                let ymxV = validateF ymx
                return $ fsCreateBou xmxV xmV ymxV ymV
getFunction :: IO (Int)
getFunction = do
              putStrLn "--- --- Generator Function --- ---"
              index <- loopVal "'1': MandelBrot | '2': Cyclic | '3': Repeated Roots | '4': 5 Real Roots" "1234"
              let indexV = validateI [index]
              return indexV
getParametersPlusFf :: IO ((Parameters),(ComplexFunction,ComplexFunction))
getParametersPlusFf = do
                       fV <- getFunction
                       putStrLn "--- --- Calculation Parameters --- ---"
                       mxIter <- getVar "Enter Maximum Iterations Per Pixel: "
                       let mxIterV = validateI mxIter
                       mxEps <- getVar "Enter Minimum Convergence distance (epsilon): "
                       let mxEpsV = validateF mxEps
                       let (ff',rc) = getff' fV
                       putStrLn "--- Rendering Method ---"
                       index <- loopVal "'1': predefined root-colours | '2': distance to root" "12"
                       let renderMethod = (case index of
                                             '1' -> do
                                                   putStrLn "--- Cutoff Method ---"
                                                   cutoff <- getVar "Enter Minimum Distance To Actual Root To Colour pixel (=epsilon default): "
                                                   let cutoffV = validateF cutoff
                                                   shadeTop <- getVar "Enter peak brightness Iteration (=max iterations default): "
                                                   let shadeTopV = validateI shadeTop
                                                   return $ fsCreateRenCutoff shadeTopV cutoffV
                                             '2' -> do
                                                   putStrLn "--- Distance Method ---"
                                                   shadeTop <- getVar "Enter peak brightness Iteration (=max iterations default): "
                                                   let shadeTopV = validateI shadeTop
                                                   return $ fsCreateRenDist shadeTopV)
                       rm <- renderMethod
                       return ((fsCreateParams rm rc mxIterV mxEpsV),ff')
getAnims :: IO [AnimationType]
getAnims = do
           putStrLn "--- --- Animation Settings --- ---"
           putStrLn "-- you can add multiple animations, select none to finish selection --"
           choice <- loopVal "'1': Zoom | '2': no Zoom" "12"
           let zoom = case choice of
                       '1' -> do
                               x <- getVar "Enter x coordinate of Zoom: "
                               let xV = validateF x
                               y <- getVar "Enter y coordinate of Zoom: "
                               let yV = validateF y
                               mf <- getVar "Enter magnification factor (2 = 2x zoom): "
                               let mfV = validateF mf
                               return $ (Zoom (xV:+yV) mfV)
                       '2' -> return None
           z <- zoom :: IO AnimationType
           choice <- loopVal "'1': shift on max Iterations | '2': shift on epsilon | '3': shift on root colours | '4': None" "1234"
           let choiceV = validateI [choice]
           step <- if choiceV /= 4 then getVar "Enter step per frame of this shifter: " else return "0"
           let stepV = validateF step
           psShifter <- return $ getShifter choiceV stepV
           return $ z:psShifter:[]
getShifter ::Int -> Double -> AnimationType
getShifter index ste =  case index of
                         1 -> (ParameterShift ([psIterations]) [ste])
                         2 -> (ParameterShift ([psEpsilon]) [ste])
                         3 -> (ParameterShift ([psRootCols]) [ste])
                         4 -> (None)
  -- Generate enum imgDim fracBound renderSettings iters eps animType =
loopVal :: String -> [Char] -> IO Char
loopVal prompt xs = do
                    putStrLn prompt
                    choice <- getLine
                    if (head choice) `elem` xs then
                       return (head choice)
                    else loopVal prompt xs

getVar :: String -> IO String
getVar prompt = do
                putStrLn prompt
                a <-getLine
                return a
choiceIO :: [a] -> [String] -> IO a
choiceIO choices prompts = undefined
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
       fs <- getFS
       if n <= 1 then
         write fs (filename ++ ".bmp")
       else
         mapM_ (simAnimate fs ) [0..n]
       --(simAnimate testSettings (Zoom (0:+0) 2) (mandelbrotFunc) (mandelbrotFunc')) [0..10]
#endif
