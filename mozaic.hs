import NewtonianMethod
import FractalSettings
import qualified ByteString.StrictBuilder as BSBS
import qualified Data.ByteString as BS
import Control.Parallel
import Colouring
import Data.Complex
import Rendering
import Codec.BMP
import Utilities
mapFractal' :: ComplexFunction -> ComplexFunction -> FractalSettings -> (Double, Double) -> (Double, Double) -> (Int, Int) -> Int -> Double -> [[BSBS.Builder]]
mapFractal' f f' fs (bx,by) (fracW, fracH) (pixW, pixH) inters eps =  [[nM (((bx + (fracXAtX x))) :+ (by + (fracYAtY y))) | x <- [0..pixW-1]] | y <- [0..pixH - 1]]
  where
        nM comDouble = applyColourFunc fs $ newtonMethod comDouble f f' inters eps
        toD = fromIntegral
        fracXAtX x= (toD x*) $ fracW / (toD pixW)
        fracYAtY y= (toD y*) $ fracH / (toD pixH)

generateImage' :: (Int,Int)-> (Int,Int)-> Int -> BS.ByteString
generateImage' (hei,wid) (xBoxes,yBoxes) frame =BSBS.builderBytes $mconcat $ mconcat $ foldr (\g acc ->  (combineListGbyRows g)++acc ) [[]] (gridOfgrids)
  where gridOfgrids = testf' (nmOnBox' (hei,wid) (xBoxes,yBoxes)) [[(x,y) |x <- [0..(xBoxes-1)]] | y <- [0..(yBoxes-1)]]
        nmOnBox' (hei,wid) (xBoxes,yBoxes) (x,y)  = nmOnBox (x,y) (xBoxes,yBoxes) (simAnimate' (createFSFromXY (hei,wid) (x,y)) frame)

nmOnBox (x,y) (xBoxes,yBoxes) fs= mapFractal' f f' fs (minfX + fracXAtX x ,minfY + fracYAtY y) (fTotalW / (toD xBoxes) , fTotalH / (toD yBoxes)) (pixTotalW `div` xBoxes , pixTotalH `div` yBoxes) inters eps
    where toD = fromIntegral
          f = fsF fs
          f' = fsF' fs
          (minfX, minfY, fTotalW,fTotalH) = (fsXBound2 fs, fsYBound2 fs,(fsXBound1 fs) - (fsXBound2 fs),(fsYBound1 fs - fsYBound2 fs))
          (pixTotalW,pixTotalH,inters,eps) =(fsWid fs, fsHei fs,fsIters fs,fsEpsilon fs)
          fracXAtX x= (toD (x * xStep)*) $ fTotalW / (toD pixTotalW)
          fracYAtY y= (toD (y * yStep)*) $ fTotalH/ (toD pixTotalH)
          xStep = pixTotalH `div` xBoxes
          yStep = pixTotalW `div` yBoxes

simAnimate' :: FractalSettings -> Int -> FractalSettings
simAnimate' fs@(FS a b c _ d) frame = nextfs
    where
        f = fsF fs
        f' = fsF' fs
        nextfs = foldr(\animI newfs ->
            case animI of
                rs@(Zoom c z)                -> zoomToRoot c z newfs frame
                (ParameterShift funcs steps) -> applyParameterShift funcs steps newfs frame

                (None)                       -> newfs)
            fs (fsAnimType fs)
zoomToRoot :: Complex Double -> Double -> FractalSettings -> Int -> FractalSettings
zoomToRoot _ _ fs 0 = fs
zoomToRoot (a:+b) zoomFactor fs frame = FS (fsFs fs) (fsDim fs) ((a + deltaX,a - deltaX),(b + deltaY, b - deltaY)) (fsParams fs) (fsAnimType fs)
  where
        curDim = (abs (fsXBound1 fs - fsXBound2 fs),abs (fsYBound1 fs - fsYBound2 fs))
        newDimension = mapTuple (curDim) (*(1/(zoomFactor * fromIntegral frame)))
        deltaX = (fst newDimension) /2
        deltaY = (snd newDimension) /2
applyParameterShift :: [ParameterModify] -> [Double] -> FractalSettings -> Int ->
                       FractalSettings
applyParameterShift funcs steps fs frame = newfs
  where
    newfs = do
            let fsSteps = zip funcs steps
            (FS (fsFs fs)
                (fsDim fs)
                (fsBound fs)
                ( foldr (\(f,s) params -> f params (s*(fromIntegral frame)))
                    (fsParams fs) fsSteps )
                (fsAnimType fs))
testf' :: ((Int,Int) -> [[BSBS.Builder]]) -> [[(Int,Int)]] -> [[ [[BSBS.Builder]] ]]
testf' f [x] = [map f x]
testf' f (x:xs) = ef `par` (etestf `par` (ef:etestf))
  where ef = (map f x) -- force was taken out due to BSBS, don't know it that's good
        etestf = testf' f xs
write' :: (Int,Int)-> (Int,Int)-> String -> Int-> IO ()
write' (hei,wid) (xBoxes,yBoxes) filename frame=  writeBMP (filename++(show frame)++".bmp") bmp >> putStrLn ("Saved:" ++ filename)
  where rgba = generateImage' (hei,wid) (xBoxes,yBoxes) frame
        bmp = packRGBA32ToBMP (wid) (hei) (rgba)

anims = [(ParameterShift [psRootCols] [25]),(ParameterShift [psUpperShader] [10]),(ParameterShift [psEpsilon] [0.0005]),(ParameterShift [psRootCols] [25]),(ParameterShift [psCutoffEps] [0.0005]),(ParameterShift [psRootCols] [50]),(ParameterShift [psIterations] [1])]

createFSFromXY :: (Int,Int) -> (Int,Int) -> FractalSettings
createFSFromXY (hei,wid) (x,y) = fsCreate (fst ffrc) (hei,wid) bounds render (snd ffrc) 40 (0.0001) anim --(f,f') imgDim fracBound renderSettings rootCols maxIters eps animType =
    where anim = [ParameterShift [psRootCols] [1],Zoom (0:+0) 2]-- :(take (y`mod`4) $drop (x`mod`3) anims)
          render = (if even y then DistanceR 40 else Cutoff 40 ((fromIntegral (x+y))/ 1000))
          bounds = ((20,-20),(20,-20))
          ffrc = getff' ((x+y)`mod`3)
combineListGbyRows :: [[[BSBS.Builder]]] -> [[BSBS.Builder]]
combineListGbyRows [] = []
combineListGbyRows (x:[]) = x
combineListGbyRows (x:y:[]) = combineGbyRows x y
combineListGbyRows (x:y:xs) = combineGbyRows (combineGbyRows x y) (combineListGbyRows xs)
combineGbyRows :: [[BSBS.Builder]] -> [[BSBS.Builder]] -> [[BSBS.Builder]]
combineGbyRows [] [] = []
combineGbyRows [[]] b = b
combineGbyRows a [[]] = a
combineGbyRows (r:rows) (r2:rows2) = [ r <> r2 ] ++ combineGbyRows rows rows2
testFs = fsGenerate 0 (100,100) ((1,-1),(1,-1)) (Cutoff 20 0.001) 20 0.001 [None]

main = do
       mapM_ (write' (1000,1000) (5,5) "Mozaic") [0..255]
       putStrLn "Done"
