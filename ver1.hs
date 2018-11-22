
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
import IOinput
import Rendering
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

-----------------------------------------------------------------------------Rendering

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
       fs <- getFS
       if n <= 1 then
         write fs (filename ++ ".bmp")
       else
         mapM_ (simAnimate fs ) [0..n]
       --(simAnimate testSettings (Zoom (0:+0) 2) (mandelbrotFunc) (mandelbrotFunc')) [0..10]
#endif
