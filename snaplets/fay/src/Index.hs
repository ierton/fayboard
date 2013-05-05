-- {-# LANGUAGE EmptyDataDecls    #-}
-- {-# OPTIONS -Wall -fno-warn-name-shadowing -fno-warn-unused-do-bind #-}

module Fayboard where

import FFI
import Prelude
import FayRef

import Types

data Document 
data Event
data JQuery

data Position


posTop :: Position -> Fay Int
posTop = ffi "%1.top"

posLeft :: Position -> Fay Int
posLeft = ffi "%1.left"


addOnload :: Fay f -> Fay ()
addOnload = ffi "window.addEventListener(\"load\", %1)"

documentReady :: (Event -> Fay ()) -> Fay ()
documentReady = ffi "jQuery(window.document)['ready'](%1)"

alert :: String -> Fay ()
alert = ffi "window.alert(%1)"

ajaxJson :: String -> (Automatic f -> Fay ()) -> Fay ()
ajaxJson = ffi "jQuery.ajax(%1, { success : %2 })"

debug = putStrLn

select :: String -> Fay JQuery
select = ffi "jQuery(%1)"

selectCard ref = select $ "#" ++ ref

getAttr :: String -> JQuery -> Fay String
getAttr = ffi "%2['attr'](%1)"

stopDrag :: JQuery -> Position -> Fay ()
stopDrag jq pos = do
  id <- getAttr "id" jq
  l <- posLeft pos
  t <- posTop pos
  putStrLn $ "stop dragging " ++ id ++ "new pos: " ++ (show (l,t))

draggable :: JQuery -> (JQuery -> Position -> Fay ()) ->  Fay ()
draggable = ffi "%1.draggable({ stack : \".card\", stop : function(e,ui) { %2(ui.helper, ui.position); } })"

appendTo :: a -> JQuery -> Fay JQuery
appendTo = ffi "%2['appendTo'](%1)"

ajaxBoard gr = ajaxJson ajax_query_path $ \g' -> do
  g <- readFayRef gr
  debug $ "update: state tag is " ++ (show $ boardTag g')
  when ((boardTag g) /= (boardTag g')) $ do
    brd <- select "#board"
    let 
      onadd (Item (x,y) (Card (Opened ref))) = do
        debug $ "gonna add item " ++ (show ref)
        card <- select $ "<img id='"++ ref ++ "' class='card ui-widget-content' src='" ++ (card2path ref) ++ "'/>"
        appendTo brd card
        draggable card stopDrag
        debug $ "added " ++ (show ref)
        return ()
      ondel (Item (x,y) (Card (Opened ref))) = do
        debug $ "gonna del item " ++ (show ref)
        return ()
      onmrg (Item _ (Card (Opened ref))) (Item (x,y) _) = do
        debug $ "gonna merge item " ++ (show ref)
        return ()
    dualscan onadd ondel onmrg (boardItems g) (boardItems g')
    writeFayRef gr g'
    debug $ "updating the state"
  ajaxBoard gr

greeter = do
  debug "Hello, Fay"

startAjax = do
  gr <- newFayRef emptyBoard
  ajaxBoard gr
  
main = mapM_ addOnload [startAjax, greeter]
  

