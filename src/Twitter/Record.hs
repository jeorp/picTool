{-# LANGUAGE TemplateHaskell, DataKinds, TypeOperators, FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}

module Twitter.Record where

import Data.Aeson
import Data.Extensible
import Control.Lens hiding ((:>))
import Data.Text

-- example
type User = Record
  [ "name" :> String,
    "sss" :> Int 
  ]

