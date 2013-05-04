-- {-# LANGUAGE EmptyDataDecls    #-}
-- {-# OPTIONS -Wall -fno-warn-name-shadowing -fno-warn-unused-do-bind #-}

module Fayboard where

import FFI
import Prelude

import Types
import FayRef

data Document 
data Event

documentReady :: (Event -> Fay ()) -> Fay ()
documentReady = ffi "jQuery(window.document)['ready'](%1)"

alert :: String -> Fay ()
alert = ffi "window.alert(%1)"

ajaxJson :: String -> (Automatic f -> Fay ()) -> Fay ()
ajaxJson = ffi "jQuery.ajax(%1, { success : %2 })"

ajaxBoard = ajaxJson ajax_update_path $ \(Board tag _) -> do
  putStrLn $ "update: state tag is " ++ (show tag)
  ajaxBoard

main = do
  documentReady $ \_ -> ajaxBoard

