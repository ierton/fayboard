{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Data.Aeson as A
import Data.Aeson.Types as A
import Data.Aeson.Generic as AG
import qualified Data.ByteString.Char8 as BS
import Data.Data
import Data.IORef
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Typeable

import Control.Concurrent (threadDelay)
import Control.Concurrent.STM
import Control.Concurrent.STM.TChan
import Control.Lens

import Snap
import Snap.Snaplet
import Snap.Snaplet.Fay
import Snap.Snaplet.Session
import Snap.Snaplet.Session.Backends.CookieSession
import Snap.Blaze
import Snap.Util.FileServe

import System.IO
import System.IO.Unsafe

import Text.Printf

import Types hiding (Fay)
import Site

debug x = liftIO $ putStrLn x

data App = App
  { _fay :: Snaplet Fay
  , _session :: Snaplet SessionManager
  }

makeLenses ''App

type SessionID = T.Text

data Session = Session
  { sessChannel :: TChan Board
  }

data SessionState = SS
  { sessions :: M.Map SessionID Session
  , latestBoard :: Board
  }

sleep_sec s = liftIO $ threadDelay (1000 * 1000 * s)

atomicReadIORef r = atomicModifyIORef r (\x -> (x,x))

-- Hack, use MonadReader or something
singletonState :: IORef SessionState
singletonState = unsafePerformIO (newIORef $ SS M.empty defaultBoard)
{-# NOINLINE singletonState #-}

-- | Respond with Internal Server Error
send500 :: Maybe String -> Handler a b c
send500 msg = do
  modifyResponse $ setResponseStatus 500 "Internal Server Error"
  writeBS $ fromMaybe "Internal Server Error" (msg >>= pure . BS.pack )
  finishWith =<< getResponse

getCurrentSession :: Handler b App (TChan Board)
getCurrentSession = with session $ do
  st <- liftIO $ atomicReadIORef singletonState
  tok <- csrfToken
  case M.lookup tok (sessions st) of
    Just (Session chan) -> return chan
    Nothing -> send500 (Just $ printf "Session %s not found" (show tok))

addCurrentSession = with session $ do
  tok <- csrfToken
  liftIO $ do
    chan <- atomically (newTChan)
    b <- atomicModifyIORef singletonState $ \(SS m b) ->
      let m' = M.insert tok (Session chan) m in (SS m' b, b)
    atomically $ writeTChan chan b
  commitSession

-- parseGeneric :: (Data a) => A.Value -> A.Parser a
-- parseGeneric val =
--   case AG.fromJSON val of
--     A.Success a -> return a
--     A.Error s -> fail $ "parseGeneric fails:" ++ s

handleSite = do
  addCurrentSession
  blaze site

handleAjax :: Handler App App ()
handleAjax = do
  chan <- getCurrentSession
  debug "session aquired"
  brd <- liftIO $ atomically $ readTChan chan
  debug $ "got board tagged with " ++ (show $ boardTag brd)
  toFayax $ return brd
  
app :: SnapletInit App App
app = makeSnaplet "app" "A snaplet example application" Nothing $ do
  f <- nestSnaplet "fay" fay initFay
  sm <- nestSnaplet "session" session $ initCookieSessionManager "config/site_key.txt" "session" (Just 3600)
  addRoutes routes
  return (App f sm)
  where
    routes =
      [ ("/", handleSite )
      , ("/fay", with fay fayServe)
      , ("/static", serveDirectory "static")
      , ("/ajax", handleAjax)
      ]

main :: IO ()
main = serveSnaplet defaultConfig app

