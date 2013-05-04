-- {-# LANGUAGE EmptyDataDecls    #-}
-- {-# OPTIONS -Wall -fno-warn-name-shadowing -fno-warn-unused-do-bind #-}

module Alert where

import FFI
import Prelude

import Types
import JQuery

main :: Fay ()
main = alert "Hello, World!"

-- | Alert using window.alert.
alert :: String -> Fay ()
alert = ffi "window.alert(%1)"

ajaxJson :: String -> (Automatic f -> Fay ()) -> Fay ()
ajaxJson = ffi "jQuery.ajax(%1, { success : %2 })"

update :: Fay ()
update =
  ajaxJson "/ajax/update" (\(Board cells) -> do
    
    )
