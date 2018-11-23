module IOinput (getFS)
where
import Rendering
import FractalSettings
import Data.Complex
import Data.Maybe
import Text.Read

inputFS :: IO FractalSettings --testSettings = FS (2000,2000) ((1,-1),(1,-1)) (Param (DistanceR 30) rootcolours 20 0.000001)
inputFS = undefined
--FS (ComplexFunction,ComplexFunction) ImageDimensions FractalBoundaries Parameters [AnimationType]
inputDefaultFS :: IO (ComplexFunction,ComplexFunction)
inputDefaultFS = undefined
reValidInt :: String -> String -> Int
reValidInt prompt string = do
                           putStrLn prompt
                           choice <- getLine
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
           let shifter =do
                         choice <- loopVal "'1': None | '2': shift on epsilon | '3': shift on colours |\n'4': shift on max Iters | '5': shift on upperShader |\n '6': shift on colouring cutoff " "123456"
                         let choiceV = validateI [choice]
                         step <- if choiceV /= 1 then getVar "Enter step per frame of this shifter: " else return "0"
                         let stepV = validateF step
                         if (choiceV == 1) then
                            return $ [getShifter choiceV stepV]
                         else do
                            shiter <- shifter
                            return $ [getShifter choiceV stepV] ++shiter
           psShifter <- shifter
           return $ z:psShifter
loopValUntill :: String -> Char -> IO AnimationType -> IO Char
loopValUntill prompt stoppingC f = do
                                 putStrLn prompt
                                 choice <- getLine
                                 if (head choice) == stoppingC then
                                   return (head choice)
                                 else loopValUntill prompt stoppingC f

getShifter ::Int -> Double -> AnimationType
getShifter index ste =  case index of
                         1 -> (None)
                         2 -> (ParameterShift ([psEpsilon]) [ste])
                         3 -> (ParameterShift ([psRootCols]) [ste])
                         4 -> (ParameterShift ([psIterations]) [ste])
                         5 -> (ParameterShift ([psUpperShader]) [ste])
                         6 -> (ParameterShift ([psCutoffEps]) [ste])
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
