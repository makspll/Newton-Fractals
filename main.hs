{-# LANGUAGE UnicodeSyntax #-}
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
       putStrLn "Maks & Michal 2k18 >>= with love >> we present you with..."
       putStrLn "----------------------------------------------------------"
       putStrLn "  __                _        _     _     _ _  _ _ _ _ _   "
       putStrLn " / _|              | |      | |    \\ \\  |  _||  _|_   _|  "
       putStrLn "| |_ _ __ __ _  ___| |_ __ _| |     \\ \\ | |_ | |_  | |    "
       putStrLn "|  _| '__/ _` |/ __| __/ _` | |     /\\ \\|  _||  _| | |    "
       putStrLn "| | | | | (_| | (__| || (_| | |    / /\\ \\ |_ | |_  | |    "
       putStrLn "|_| |_|  \\__,_|\\___|\\__\\__,_|_|   /_/  \\_\\__||___| |_|    "
       putStrLn "----------------------------------------------------------"
       putStrLn "   the ultimate newton method fractal yeeting software ©  "
       putStrLn "----------------------------------------------------------"
       putStrLn "To begin the ultimate yeet experience, enter no.of frames:"
       n <- getLine
       let nV = read n
       fs <- getFS
       if nV <= 1 then
         write fs ("fractal" ++ ".bmp")
       else
         mapM_ (simAnimate fs ) [0..nV]
#endif
