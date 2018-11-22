
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
import qualified Window as Win
#endif


defaultWindowSize = (500,500)
defaultSlice = ((10,-10),(10,-10))
filename = "fractal"
----------------------------------------------------------------Rendering

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
