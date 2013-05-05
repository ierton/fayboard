{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE CPP #-}

module Types where

import Data.Data
import Prelude

#ifdef FAY
import FFI
#endif

ajax_query_path = "/ajax/update"

data StateTag = StateTag Int
  deriving(Eq, Show, Data, Typeable)

data Visibility = Opened String | Closed
  deriving(Show, Data, Typeable)

card2path c = "/static/img/small/" ++ c ++ ".png"

data ItemType = Card Visibility
              | Deck Visibility
  deriving(Show, Data, Typeable)

data Item = Item (Int,Int) ItemType
  deriving(Show, Data, Typeable)

data Board = Board
  { boardTag :: StateTag
  , boardItems :: [Item]
  }
  deriving(Show, Data, Typeable)

emptyBoard = Board (StateTag 0) []

defaultBoard = Board (StateTag 1) items where
  items = map card ["card0", "card1", "card2", "card3", "card4", "card5" ]
  card x = Item (0,0) (Card (Opened x))

#ifndef FAY
type Fay a = IO a
#endif

compareItem (Item (x,y) _) (Item (x',y') _) =
  case x`compare`x' of
    EQ -> y`compare`y'
    neq -> neq

dualscan :: (Item -> Fay ()) -> (Item -> Fay ()) -> (Item -> Item -> Fay ()) -> [Item] -> [Item] -> Fay ()
dualscan onadd ondel onmrg o n = dualscan' o n
  where
    dualscan' [] ns = mapM_ onadd ns
    dualscan' os [] = mapM_ ondel os
    dualscan' o'@(o:os) n'@(n:ns) =
      case o`compareItem`n of
        EQ -> onmrg o n >> dualscan' os ns
        GT -> onadd n >> dualscan' o' ns
        LT -> ondel o >> dualscan' os n'

test_dualscan a b = dualscan (prnt "add:") (prnt "del:") (prnt2 "mrg:") a b
  where prnt s x = do
          putStrLn $ s ++ (show x)
        prnt2 s a b = do
          putStrLn $ s ++ (show a) ++ " , " ++ (show b)

