{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Site where

import Text.Printf

import Data.Data
import Data.Typeable

import Text.Blaze as B
import Text.Blaze.Html5 as B
import Text.Blaze.Html5.Attributes as B (draggable, rel, media, action, enctype, href, name,
                                         size, type_, value, class_, src, id)

linkcss (n::String) = B.link ! B.rel "stylesheet"
      ! B.href (B.toValue ("/static/css/" ++ n))
      ! B.type_ "text/css" ! B.media "all"

site = B.html $ do
  B.head $ do
    B.title "Hello, snap"
    linkcss "Styles.css"
    B.link ! B.rel "stylesheet" ! B.href "http://code.jquery.com/ui/1.10.3/themes/smoothness/jquery-ui.css"
    B.script ! B.src "http://code.jquery.com/jquery-1.9.1.js" $ return ()
    B.script ! B.src "http://code.jquery.com/ui/1.10.3/jquery-ui.js" $ return ()
    B.script ! B.src "/static/js/JQueryDD.js" $ return ()
    B.script ! B.src "/fay/Index.js" $ return ()

  B.body $ do
    B.div $ B.h1 "Hello Snap, Fay, Blaze, JQuery"
    B.div ! B.id "board" $ do
      return ()

    -- B.div $ do
    --   let card (x :: String) = B.img ! B.src (B.toValue (printf "/static/img/small/%s.png" x :: String))
    --             ! B.id (B.toValue x) ! B.class_ "card ui-widget-content"
    --   card "card0"
    --   card "card1"
    --   card "card2"
    --   card "card3"
    --   card "card4"
    --   card "card5"
    B.div ! B.class_ "debug" $ return ()

