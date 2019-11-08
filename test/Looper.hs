{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
module Looper where

import Data.Generics.Labels ()
import GHC.Generics
import Schemas

data Looper
  = Number Int
  | Loop Looper
  deriving (Eq, Generic, Show)

instance HasSchema Looper where
  schema = named "Looper" $ oneOf
    [ altWith schema #_Number
    , altWith schema #_Loop
    ]

looper1 = Number 1
looper2 = Loop $ Number 2
