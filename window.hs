
module Window ( create )

where

import Control.Monad
import Control.Monad.IO.Class
import Data.IORef
import Graphics.UI.Gtk
import Data.List
import Data.Complex
import qualified Data.ByteString as BS
import Foreign.Marshal.Array ( newArray )
import Foreign.C.Types

import FractalSettings

imageDimX = 150
imageDimY = imageDimX

createWindow :: IO Window
createWindow = do
    win <- windowNew
    set win [ windowTitle           := "Kickass fractals"
            , windowResizable       := False
            , windowDefaultWidth    := 1000
            , windowDefaultHeight   := 500 ]

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
    let c = length $ filter (=='.') str1
    let str2 = (removeMinuses False) $ removeDots isInt str1
    let str3 = if isSelected then str2 else fillZero str2
    entrySetText field str3
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


colours = [(255,0,0),(0,255,255),(0,255,0)]
roots = [(1:+0),((-0.5):+sqrt(3)/2),((-0.5):+((-sqrt(3))/2))]
rootcolours = zip roots colours
filename = "fractal"

fractalMaxMinX = (1,-1)
fractalMaxMinY = (1,-1)
imageSize = (2000,2000)
iterations = 30 :: Int
shadingMaxIter = 30
epsilon = 0.000001
rootColorThreshold = 0.000001
colourMode = "DistanceR"

createFsFromWidgets :: (Entry, Entry) -> (Entry, Entry) -> Entry -> Entry -> [RadioButton] -> IO FractalSettings
createFsFromWidgets (xMinW, xMaxW) (yMinW, yMaxW) epsW iteW rbutWs = do
    mapM_ (formatField False False) [xMinW , xMaxW, yMinW, yMaxW, epsW]
    formatField True False iteW
    xMinText <- (entryGetText xMinW) :: IO String ;  let xMin = read xMinText :: Double
    xMaxText <- (entryGetText xMaxW) :: IO String ;  let xMax = read xMaxText :: Double
    yMinText <- (entryGetText yMinW) :: IO String ;  let yMin = read xMinText :: Double
    yMaxText <- (entryGetText yMaxW) :: IO String ;  let yMax = read xMaxText :: Double
    epsText  <- (entryGetText epsW)  :: IO String ;  let eps  = read epsText  :: Double
    iteText  <- (entryGetText iteW)  :: IO String ;  let ite  = read iteText  :: Int

    let testSettings2 = FS (imageDimX,imageDimY) ((xMax,xMin),(yMax,yMin)) (Param (Cutoff ite eps) rootcolours (if ite > 20 then ite else 20) eps) (None)
    return testSettings2

startAnimation fs state = do
    (aid, frame, fsToBmp, image) <- readIORef state
    let naid = aid + 1
    writeIORef state (naid, frame, fsToBmp, image)
    timeoutAdd (animate naid state) 1000
    where
        animate aid state = do
            (caid, cframe, fsToBmp, image) <- readIORef state
            putStr (show caid)
            if caid /= aid then return False
            else do
                let bmp = fsToBmp fs
                imgPtr <- newArray (map CUChar (BS.unpack bmp))
                pixbuf <- pixbufNewFromData imgPtr
                                            ColorspaceRgb
                                            True 8
                                            imageDimX imageDimY
                                            (imageDimX * 4)
                imageSetFromPixbuf image pixbuf
                return True


create :: (FractalSettings -> BS.ByteString) -> IO Window
create fsToBmp = do
    void initGUI

    win <- createWindow

    grid <- tableNew 5 5 False

    -- image fractalboundries ((Double, Double),(Double, Double))

    createLabel "X boundries" >>= attach grid 0 1 1 1
    xMinW <- createDoubleEntryField "-1.0" >>= attach grid 1 1 1 1
    xMaxW <- createDoubleEntryField "1.0" >>= attach grid 2 1 1 1

    createLabel "Y boundries" >>= attach grid  0 2 1 1
    yMinW <- createDoubleEntryField "-1.0" >>= attach grid 1 2 1 1
    yMaxW <- createDoubleEntryField "1.0" >>= attach grid 2 2 1 1

    -- render parameters (Render, rootcolours, Int - iteracje, Double -distance / cutoff)
    createLabel "Epsilon" >>= attach grid 0 3 1 1
    epsW <- createDoubleEntryField "0.000001" >>= attach grid 1 3 1 1
    createLabel "Iterations" >>= attach grid 2 3 1 1
    iteW <- createIntEntryField "20" >>= attach grid 3 3 1 1

    createLabel "Drawing method" >>= attach grid 0 4 1 1
    r1 <- createRadioButton "Normal" >>= attach grid 1 4 1 1
    r2 <- createRadioButton "420" >>= attach grid 3 4 1 1
    radioButtonSetGroup r1 r2

    -- drawing

    pixbuf <- pixbufNew ColorspaceRgb True 8 imageDimX imageDimY
    pixbufFill pixbuf 255 255 255 255
    image <- imageNewFromPixbuf pixbuf
    tableAttach grid image 5 6 0 6 [Fill] [Fill] 2 2

    state <- (newIORef (0, 0, fsToBmp, image))

    renderButton <- createButton "Do The *MAGIC*" >>= attach grid 0 5 5 1
    on renderButton buttonActivated $ do
        fs <- createFsFromWidgets (xMinW, xMaxW) (yMinW, yMaxW) epsW iteW [r1,r2]
        startAnimation fs state
        return ()
    containerAdd win grid
    widgetShowAll win

    mainGUI
    return win
    where
        attach pare x y w h chil = do
            tableAttach pare chil x (x+w) y (y+h) [Fill] [Fill] 0 0
            return chil
