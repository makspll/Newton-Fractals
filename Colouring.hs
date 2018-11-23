module Colouring (applyColourFunc,colourWithDToR,colourWithCutOff,mixRGB)

where

import FractalSettings
import Data.Complex
import Data.Word
import Utilities
import qualified ByteString.StrictBuilder as BSBS
mixRGB :: ColorW8 -> ColorW8 -> ColorW8
mixRGB (c1,c2,c3) (a1,a2,a3) = mapTuple3 ((c1r+a1r),(c2r+a2r),(c3r+a3r)) (\x-> clamp x (255,0) )
  where c1r = fromIntegral c1
        c2r = fromIntegral c2
        c3r = fromIntegral c3
        a1r = fromIntegral a1
        a2r = fromIntegral a2
        a3r = fromIntegral a3


applyColourFunc :: FractalSettings -> (Complex Double,Int) -> BSBS.Builder
applyColourFunc fs (root,ite) = case (fsRendS fs) of
                                    (Cutoff i d)   -> colourWithCutOff (fsRootCols fs) (root,ite) d i
                                    (DistanceR i)  -> colourWithDToR (fsRootCols fs) (root,ite) i
    where the [] = error "noColours found for"
          the [x] = x

colourWithCutOff :: RootCols -> (Complex Double, Int) -> Double -> Int -> BSBS.Builder
colourWithCutOff _ (_,0) _ _  = mconcat [BSBS.word8 0,BSBS.word8 0,BSBS.word8 0,BSBS.word8 255]
colourWithCutOff rootColours (root,ite) cutoff maxShadeIter  = mconcat [BSBS.word8 r,BSBS.word8 g,BSBS.word8 b,BSBS.word8 255]
  where scaledIte = scaleVar (fromIntegral ite) (fromIntegral maxShadeIter,0) (1,0)
        filteredByEpsilon = filter (\(r,c) -> abs(realPart (r - root)) < cutoff && abs(imagPart (r - root)) < cutoff) rootColours
        (r,g,b) = clampLoopedRGB $ mapTuple3 (snd $ the filteredByEpsilon)  (\x -> round $(fromIntegral x)* scaledIte) --bound this
        the [] = ((0:+0),(0,0,0)) --If not converged but not to any root, paint it black
        the [x] = x
        the ((r,c):xs) = (r,(foldr (\(r,c) c2-> mixRGB c c2) c (xs))) -- if within range of multiple roots mix colours

colourWithDToR :: RootCols -> (Complex Double, Int) -> Int -> BSBS.Builder
colourWithDToR _ (_,0) _ = mconcat [BSBS.word8 0,BSBS.word8 0,BSBS.word8 0,BSBS.word8 255]
colourWithDToR rootColours (root,ite) maxShadeIter =  mconcat [BSBS.word8 r,BSBS.word8 g,BSBS.word8 b,BSBS.word8 255]
  where scaledIte = scaleVar (fromIntegral ite) (fromIntegral maxShadeIter,0) (1,0)
        switchToDist = map (\(r,c) -> (((distanceFactor r root):+0),(c))) rootColours :: [((Complex Double),(ColorW8))]
        distanceFactor rt1 rt2 = (scaleVar (magnitude ((rt1) - rt2)) (100,0) (0,1))
        (r,g,b) =clampLoopedRGB$ foldr1 (\c c2-> mixRGB c c2) [mapTuple3 (cr,cg,cb) (\x-> round $(fromIntegral x)*df *scaledIte)| ((df:+nan),(cr,cg,cb))<-switchToDist] --bound this
