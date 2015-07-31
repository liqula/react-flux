module React.Flux.JsTypes where

import Data.Aeson

#ifndef __GHCJS__
type JSRef a = ()
type Callback a = JSRef a
type Export a = JSRef a
#endif

-- | This type is for the return value of React.createClass.
newtype ReactClassRef props = ReactClassRef (JSRef ())

-- | This type is for the return value of React.createElement
newtype ReactElementRef = ReactElementRef (JSRef ())
