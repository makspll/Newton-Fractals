
module Window ( create )

where

import Control.Monad
import Control.Monad.IO.Class
import Data.IORef
import Graphics.UI.Gtk as Gtk

createWindow :: IO Window
createWindow = do
    win <- Gtk.windowNew
    set win [ windowTitle           := "Kickass fractals"
            , windowResizable       := False
            , windowDefaultWidth    := 1000
            , windowDefaultHeight   := 500 ]

    on win deleteEvent $ do 
        liftIO mainQuit
        return False
    return win

createButton :: String -> IO Button
createButton str = do
    button <- buttonNew
    set button [ buttonLabel := str ]
    return button

create = do
    void Gtk.initGUI 

    win <- createWindow 

    grid <- tableNew 5 10 True

    quitButton <- createButton "Twoja stara"
    attach grid quitButton 1 1 2 2

    containerAdd win grid
    widgetShowAll win
    Gtk.mainGUI
    return win
    where
        attach pare chil x y w h = tableAttach pare chil x (x+w) y (y+h) [] [] 2 2


