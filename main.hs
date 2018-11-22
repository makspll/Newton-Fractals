
import Control.Concurrent
import NewtonianMethod
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
#ifdef __GUI_APP

main = do
    win <- Win.create 
    return (0)

#else

main = do
       getNumCapabilities >>= setNumCapabilities
       putStrLn "Test Mode, generating Test Fractal"
       let n = 50
       fs <- getFS
       if n <= 1 then
         write fs (filename ++ ".bmp")
       else
         mapM_ (simAnimate fs ) [0..n]
#endif
