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
  [ "created_at" :> Text,
    "description" :> Text,
    "followers_count" :> Int,
    "following" :> Bool,
    "id_str" :> Text,
    "name" :> Text,
    "screen_name" :> Text

  ]

created_at :: Associated s ("created_at" ':> Text) => Lens' (Record s) Text 
created_at = #created_at

description :: Associated s ("description" ':> Text) => Lens' (Record s) Text 
description = #description

following_count :: Associated s ("following_count" ':> Text) => Lens' (Record s) Text 
following_count  = #following_count 

following :: Associated s ("following" ':> Text) => Lens' (Record s) Text 
following = #following 

id_str :: Associated s ("id_str" ':> Text) => Lens' (Record s) Text 
id_str = #id_str

name :: Associated s ("name" ':> Text) => Lens' (Record s) Text 
name = #name

screen_name :: Associated s ("screen_name" ':> Text) => Lens' (Record s) Text 
screen_name = #screen_name