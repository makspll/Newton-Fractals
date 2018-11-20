
module Window ( create )

where

import Control.Monad
import Control.Monad.IO.Class
import Data.IORef
import Graphics.UI.Gtk 
import Data.List

createWindow :: IO Window
createWindow = do
    win <- windowNew
    set win [ windowTitle           := "Kickass fractals"
            , windowResizable       := True
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

do_theShit :: Entry -> Int -> String -> Int -> IO ()
do_theShit field i s j = do
    str <- entryGetText field :: IO String
    let str1 = filter (\c -> c `elem` ['0'..'9'] || c == '.') str
    let c = length $ filter (=='.') str1
    let str2 = removeDots False str1
    entrySetText field str2
    putStr str1
    where
        removeDots _ [] = []
        removeDots False (c:str) | c == '.' = c : removeDots True str
                                 | otherwise = c : removeDots False str
        removeDots True (c:str) | c == '.' = removeDots True str
                                | otherwise = c : removeDots True str

createTextField :: String -> IO Entry
createTextField str = do
    field <- entryNew
    set field   [ entryMaxLength := 10
                , entryEditable := True
                , entryText := str
                , entryXalign := 1 ]
    buffer <- entryGetBuffer field
    on buffer entryBufferInsertedText (do_theShit field)
    return field

createButton :: String -> IO Button
createButton str = do
    button <- buttonNew
    set button [ buttonLabel := str ]
    return button


create = do
    void initGUI 

    win <- createWindow 

    grid <- tableNew 5 5 False

    -- image dimensions (Int , Int)

    let imageDimensions = (100,100)

    -- image fractalboundries ((Double, Double),(Double, Double))
    
    createLabel "X boundries" >>= attach grid 0 1 1 1
    createTextField "0.0" >>= attach grid 1 1 1 1
    createTextField "1.0" >>= attach grid 2 1 1 1
    
    createLabel "Y boundries" >>= attach grid  0 2 1 1
    createTextField "0.0" >>= attach grid 1 2 1 1
    createTextField "1.0" >>= attach grid 2 2 1 1 

    -- render parameters (Render, rootcolours, Int - iteracje, Double -distance / cutoff)
    createLabel "Epsilon" >>= attach grid 0 3 1 1
    createTextField "0.0001" >>= attach grid 1 3 1 1
    createLabel "Iterations" >>= attach grid 2 3 1 1
    createTextField "20" >>= attach grid 3 3 1 1 
    
    createLabel "Drawing method" >>= attach grid 0 4 1 1
    r1 <- createRadioButton "Normal"
    r2 <- createRadioButton "420"
    radioButtonSetGroup r1 r2
    attach grid 1 4 1 1 r1
    attach grid 3 4 1 1 r2

    
    -- animation type

    createButton "Do The *MAGIC*" >>= attach grid 0 5 5 1

    -- drawing

    pixbuf <- pixbufNew ColorspaceRgb True 8 100 100
    pixbufFill pixbuf 255 255 255 255
    image <- imageNewFromPixbuf pixbuf 
    attach grid 5 0 1 5 image

    containerAdd win grid
    widgetShowAll win
    mainGUI
    return win
    where
        attach pare x y w h chil = tableAttach pare chil x (x+w) y (y+h) [Fill] [Fill] 5 5


