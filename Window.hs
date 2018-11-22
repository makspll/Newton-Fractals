
module Window ( create )

where

import Control.Monad
import Control.Monad.IO.Class
import Control.DeepSeq
import Data.IORef
import Graphics.UI.Gtk
import Data.List
import Data.Complex
import qualified Data.ByteString as BS
import Foreign.Marshal.Array ( newArray )
import Foreign.C.Types

import FractalSettings

imageDimX = 200
imageDimY = imageDimX

createWindow :: IO Window
createWindow = do
    win <- windowNew
    set win [ windowTitle           := "Kickass fractals"
            , windowResizable       := False]

    on win deleteEvent $ do
        liftIO mainQuit
        return False
    return win

createRadioButton :: String -> IO RadioButton
createRadioButton str = do
    button <- radioButtonNewWithLabel str
    return button

createLabel :: String -> IO Label
createLabel str =
    labelNew (Just str)

-- Do not ever assume user input
-- input should be int
-- input is currently selected
formatField :: Bool -> Bool -> Entry -> IO ()
formatField isInt isSelected field = do
    str <- entryGetText field :: IO String
    let str1 = filter (\c -> c `elem` ['0'..'9'] || c `elem` ".-" ) str
    let str2 = (removeMinuses False) $ removeDots isInt str1
    let str3 = if isSelected then str2 else (fillZero.removeHeadZeros) str2
    let str4 = if (not isSelected) && isInt then absolulify str3 else str3
    entrySetText field str4
    where
        fillZero [] = []
        fillZero (c:str) | c == '-' = c : fillZero str
                         | c == '.' = '0' : c : str
                         | otherwise = c:str
        removeMinuses _ [] = []
        removeMinuses False (c:str) = c : removeMinuses True str
        removeMinuses True (c:str) | c == '-' = removeMinuses True str
                                   | otherwise = c : removeMinuses True str
        removeDots _ [] = []
        removeDots False (c:str) | c == '.' = c : removeDots True str
                                 | otherwise = c : removeDots False str
        removeDots True (c:str) | c == '.' = removeDots True str
                                | otherwise = c : removeDots True str
        removeHeadZeros [] = []
        removeHeadZeros (c:str) | c == '0' = removeHeadZeros str
                                | c == '-' = c : (removeHeadZeros str)
                                | otherwise = (c:str)
        absolulify [] = "1"
        absolulify (c:str) | c == '-' = "1"
                           | otherwise = (c:str)

createDoubleEntryField = createNumberEntryField False
createIntEntryField = createNumberEntryField True

createNumberEntryField :: Bool -> String -> IO Entry
createNumberEntryField b str = do
    field <- entryNew
    set field   [ entryMaxLength := 10
                , entryEditable := True
                , entryText := str
                , entryXalign := 1 ]
    buffer <- entryGetBuffer field
    on buffer entryBufferInsertedText (func field)
    return field
    where
        func :: Entry -> Int -> String -> Int -> IO()
        func f x s y= formatField b True f

createButton :: String -> IO Button
createButton str = do
    button <- buttonNew
    set button [ buttonLabel := str ]
    return button

createToggleButton :: String -> IO CheckButton
createToggleButton label = do 
    button <- checkButtonNew
    set button [ buttonLabel := label ]
    return button

createFsFromWidgets :: (Entry, Entry) -> (Entry, Entry) -> Entry -> Entry -> [RadioButton] -> (CheckButton, Entry, Entry) -> (CheckButton, Entry) -> IO (FractalSettings, Int)
createFsFromWidgets (xMinW, xMaxW) (yMinW, yMaxW) epsW iteW rbutWs (zoomToggle, xZoom, yZoom) (animToggle, animFrames)
    = do
    mapM_ (formatField False False) [xMinW , xMaxW, yMinW, yMaxW, epsW, xZoom, yZoom]
    mapM_ (formatField True False) [iteW, animFrames]
    xMinText <- (entryGetText xMinW) :: IO String ;  let xMin = read xMinText :: Double
    xMaxText <- (entryGetText xMaxW) :: IO String ;  let xMax = read xMaxText :: Double
    yMinText <- (entryGetText yMinW) :: IO String ;  let yMin = read xMinText :: Double
    yMaxText <- (entryGetText yMaxW) :: IO String ;  let yMax = read xMaxText :: Double
    epsText  <- (entryGetText epsW)  :: IO String ;  let eps  = read epsText  :: Double
    iteText  <- (entryGetText iteW)  :: IO String ;  let ite  = read iteText  :: Int
    zoom <- toggleButtonGetActive zoomToggle 
    xZoomT   <- (entryGetText xZoom) :: IO String ; let xZ    = read xZoomT   :: Double
    yZoomT   <- (entryGetText yZoom) :: IO String ; let yZ    = read yZoomT   :: Double
    let zoomParam = if zoom then (Zoom (xZ :+ yZ) 1.5) else None
    enum <- whichButton rbutWs

    animate <- toggleButtonGetActive animToggle
    frText <- (entryGetText animFrames) :: IO String ; let ftemp = read frText :: Int
    let frames = if animate then ftemp else 1 

    let testSettings2 = fsGenerate enum 
            (imageDimX,imageDimY) 
            ((xMax,xMin),(yMax,yMin)) 
            (Cutoff ite eps)
            (if ite > 20 then ite else 20) eps 
            [zoomParam]
    return (testSettings2, frames)
    where
        whichButton [r0, r1, r2, r3] = do
            a0 <- toggleButtonGetActive r0
            a1 <- toggleButtonGetActive r1
            a2 <- toggleButtonGetActive r2
            a3 <- toggleButtonGetActive r3
            if a0 then return 0 else 
                if a1 then return 1 else
                    if a2 then return 2 else
                        if a3 then return 3 else return 0

startAnimation (fs, frames) state fsToBmp = 
    let bmps = map (fsToBmp fs) [0..frames]
    in
    writeIORef state (bmps, 0, frames)

animate state image = do
    (bmps, tempFrame, frames) <- readIORef state
    let frame = tempFrame `mod` frames
    let bmp = bmps !! frame
    writeIORef state (bmps, frame + 1, frames)
    imgPtr <- newArray (map CUChar (BS.unpack bmp))
    pixbuf <- pixbufNewFromData imgPtr
                                ColorspaceRgb
                                True 8
                                imageDimX imageDimY
                                (imageDimX * 4)
    imageSetFromPixbuf image pixbuf
    return True

create :: (FractalSettings -> Int -> BS.ByteString) -> IO Window
create fsToBmp = do
    void initGUI

    win <- createWindow

    grid <- tableNew 5 7 False

    createLabel "X boundries" >>= attach grid 0 1 1 1
    xMinW <- createDoubleEntryField "-1.0" >>= attach grid 1 1 1 1
    xMaxW <- createDoubleEntryField "1.0" >>= attach grid 2 1 1 1

    createLabel "Y boundries" >>= attach grid  0 2 1 1
    yMinW <- createDoubleEntryField "-1.0" >>= attach grid 1 2 1 1
    yMaxW <- createDoubleEntryField "1.0" >>= attach grid 2 2 1 1

    createLabel "Epsilon" >>= attach grid 0 3 1 1
    epsW <- createDoubleEntryField "0.000001" >>= attach grid 1 3 1 1
    createLabel "Iterations" >>= attach grid 2 3 1 1
    iteW <- createIntEntryField "20" >>= attach grid 3 3 1 1

    createLabel "Graph" >>= attach grid 0 4 1 1
    r0 <- createRadioButton "Mandel Brot" >>= attach grid 1 4 1 1
    r1 <- createRadioButton "Cyclic" >>= attach grid 2 4 1 1
    r2 <- createRadioButton "Two Rep" >>= attach grid 3 4 1 1
    r3 <- createRadioButton "Fire" >>= attach grid 4 4 1 1
    radioButtonSetGroup r1 r0
    radioButtonSetGroup r2 r0
    radioButtonSetGroup r3 r0

    createLabel "Zoom: " >>= attach grid 0 5 1 1
    zoomButton <- createToggleButton "Zoom" >>= attach grid 1 5 1 1
    xZoom <- createDoubleEntryField "0.0" >>= attach grid 2 5 1 1
    yZoom <- createDoubleEntryField "0.0" >>= attach grid 3 5 1 1
    
    createLabel "Animate: " >>= attach grid 0 6 1 1
    animateButton <- createToggleButton "Aniamte" >>= attach grid 1 6 1 1
    createLabel "Frames: " >>= attach grid 2 6 1 1
    framesNo <- createIntEntryField "5" >>= attach grid 3 6 1 1

    -- drawing

    pixbuf <- pixbufNew ColorspaceRgb True 8 imageDimX imageDimY
    pixbufFill pixbuf 255 255 255 255
    image <- imageNewFromPixbuf pixbuf
    tableAttach grid image 5 6 0 6 [Fill] [Fill] 2 2

    state <- (newIORef ([], 0, 0))

    renderButton <- createButton "Do The *MAGIC*" >>= attach grid 0 7 5 1
    on renderButton buttonActivated (updateImage state (xMinW, xMaxW) (yMinW, yMaxW) epsW iteW [r0,r1,r2,r3] (zoomButton, yZoom, xZoom) (animateButton, framesNo) )

    (updateImage state (xMinW, xMaxW) (yMinW, yMaxW) epsW iteW [r0,r1,r2,r3] (zoomButton, yZoom, xZoom) (animateButton, framesNo) )

    containerAdd win grid
    widgetShowAll win

    timeoutAdd (animate state image) 1000

    mainGUI
    return win
    where
        attach pare x y w h chil = do
            tableAttach pare chil x (x+w) y (y+h) [Fill] [Fill] 0 0
            return chil
        updateImage state xs ys eps it rs zms ams = do
            fs <- createFsFromWidgets xs ys eps it rs zms ams
            startAnimation fs state fsToBmp
            return ()
