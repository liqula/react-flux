module React.Flux.DOM where

import Data.Aeson
import Data.Aeson.Types (Pair)

import React.Flux.Element
import React.Flux.Events

div_ :: [Pair] -> ReactElementM eventHandler a -> ReactElementM eventHandler a
div_ attrs = el "div" attrs []

input_ :: String -> [Pair] -> [EventHandler eventHandler] -> ReactElementM eventHandler ()
input_ ty attrs handlers = el "input" ("type" .= ty : attrs) handlers mempty
