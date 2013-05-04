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

ajax_update_path = "/ajax/update"

data Visible = Opened String | Hidden
  deriving(Show, Data, Typeable)

data Card = Card Visible (Int,Int) 
  deriving(Show, Data, Typeable)

data StateTag = StateTag Int
  deriving(Show, Data, Typeable)

data Board = Board StateTag [Card]
  deriving(Show, Data, Typeable)

