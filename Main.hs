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

import Control.Concurrent (threadDelay)

import qualified Data.Text as T

import Snap
import Snap.Snaplet
import Snap.Snaplet.Fay
import Snap.Snaplet.Session
import Snap.Snaplet.Session.Backends.CookieSession
import Snap.Blaze
import Snap.Util.FileServe

import qualified Data.ByteString.Char8 as BS
import Data.IORef
import Control.Lens

import Text.Printf

import Text.Blaze as B
import Text.Blaze.Html5 as B
import Text.Blaze.Html5.Attributes as B (draggable, rel, media, action, enctype, href, name,
                                         size, type_, value, class_, src, id)

import Types

data App = App
  { _fay :: Snaplet Fay
  , _session :: Snaplet SessionManager
  }

makeLenses ''App

sleep_sec s = liftIO $ threadDelay (1000 * 1000 * s)

parseGeneric :: (Data a) => A.Value -> A.Parser a
parseGeneric val =
  case AG.fromJSON val of
    A.Success a -> return a
    A.Error s -> fail $ "parseGeneric fails:" ++ s

routes =
  [ ("/", handleSite )
  , ("/fay", with fay fayServe)
  , ("/static", serveDirectory "static")
  , ("/ajax", handleAjax)
  ]

handleSite = do
  rq <- getRequest
  with session $ do
    setInSession "sid" (T.pack $ BS.unpack $ rqRemoteAddr rq)
    commitSession
  blaze site

handleAjax :: Handler App App ()
handleAjax = do
  r <- getRequest
  with session $ do
    tok <- csrfToken
    liftIO $ putStrLn $ "sid: " ++ show tok
  -- A deley emulating wait for next event
  sleep_sec 60
  toFayax $ do
    return $ Board (StateTag 33) []
  
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

    B.div $ do
      let card (x :: String) = B.img ! B.src (B.toValue (printf "/static/img/small/%s.png" x :: String))
                ! B.id (B.toValue x) ! B.class_ "card ui-widget-content"
      card "card0"
      card "card1"
      card "card2"
      card "card3"
      card "card4"
      card "card5"
    B.div ! B.class_ "debug" $ return ()

app :: SnapletInit App App
app = makeSnaplet "app" "A snaplet example application" Nothing $ do
  f <- nestSnaplet "fay" fay initFay
  sm <- nestSnaplet "session" session $ initCookieSessionManager "config/site_key.txt" "session" (Just 3600)
  addRoutes routes
  return $ App f sm

main :: IO ()
main = serveSnaplet defaultConfig app

