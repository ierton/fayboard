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

data Object = Player | Wall | Item
  deriving(Show, Data, Typeable)

data Cell = Cell Int Int [Object]
  deriving(Show, Data, Typeable)

data Board = Board
  { flds :: [[Cell]]
  }
  deriving(Show, Data, Typeable)

