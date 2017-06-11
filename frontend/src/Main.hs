{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Main where

import Control.DeepSeq (NFData)
import GHC.Generics (Generic)

import React.Flux

import qualified Graph as T

data Action = Click deriving Generic

instance NFData Action

data AppState = AppState {
  graph :: T.Graph
  }

store :: ReactStore AppState
store = mkStore (AppState T.empty)

dispatch :: Action -> [SomeStoreAction]
dispatch a = [ SomeStoreAction store a ]

instance StoreData AppState where
  type StoreAction AppState = Action

  transform Click state = return state

key :: String -> [PropertyOrHandler handler] -> [PropertyOrHandler handler]
key str = (("key" $= str) :)

render :: AppState -> ReactElementM ViewEventHandler ()
render state = do
  let attrs = [ "width" $= 600
              , "height" $= 500
              , onClick $ \ev mev -> dispatch Click ]
  svg_ (key "thesvg" attrs) mempty


app :: ReactView ()
app = defineControllerView "app" store $ \state () -> render state

main :: IO ()
main = do
  -- executeAction (SomeStoreAction store Init)  
  reactRender "app" app ()

