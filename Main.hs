{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Data.Data
import Data.Typeable

import Data.Aeson as A
import Data.Aeson.Types as A
import Data.Aeson.Generic as AG

import Snap
import Snap.Snaplet
import Snap.Snaplet.Fay
import Snap.Blaze
import Snap.Util.FileServe

import Data.ByteString as BS
import Data.IORef
import Control.Lens

import Text.Printf

import Text.Blaze as B
import Text.Blaze.Html5 as B
import Text.Blaze.Html5.Attributes as B (draggable, rel, media, action, enctype, href, name, size, type_, value, class_, src, id)

import Types

data App = App { _fay :: Snaplet Fay }

makeLenses ''App

parseGeneric :: (Data a) => A.Value -> A.Parser a
parseGeneric val =
  case AG.fromJSON val of
    A.Success a -> return a
    A.Error s -> fail $ "parseGeneric fails:" ++ s

routes =
  [ ("/", blaze site )
  , ("/fay", with fay fayServe)
  , ("/static", serveDirectory "static")
  ]

linkcss (n::String) = B.link ! B.rel "stylesheet"
      ! B.href (B.toValue ("/static/css/" ++ n))
      ! B.type_ "text/css" ! B.media "all"

linkjs (n::String) = B.script ! B.type_ "text/javascript"
      ! B.src (B.toValue ("/static/js/" ++ n)) $ return ()

site = B.html $ do
  B.head $ do
    B.title "Hello, snap"

    linkcss "Styles.css"

    -- B.script ! B.type_ "text/javascript" ! B.src "/fay/Index.js" $ return ()
  B.body $ do
    B.div $ B.p "This is a header"
    B.div $ B.h1 "Hello, Snap+Blaze+Fay"

    let card (x :: String) = B.img ! B.src (B.toValue (printf "/static/img/small/%s.png" x :: String))
                                   ! B.id (B.toValue x) ! B.class_ "drag"

    card "card0"
    card "card1"
    card "card2"
    card "card3"
    card "card4"
    card "card5"

    -- card "card1" $ return ()
    -- card "card2" $ return ()
    -- B.table $
    --   forM_ [0..9] $ \(i :: Int) -> do
    --     B.tr $ do
    --       forM_ [0..9] $ \(j :: Int) -> do
    --         B.td ! (B.id (B.toValue $ (printf "%d_%d" i j :: String))) $ do

    linkjs "DragDrop.js"
    B.div ! B.class_ "debug" $ B.p "This is a footer"

app :: SnapletInit App App
app = makeSnaplet "app" "A snaplet example application" Nothing $ do
  fay' <- nestSnaplet "fay" fay initFay
  addRoutes routes
  return $ App { _fay = fay' }

main :: IO ()
main = serveSnaplet defaultConfig app


