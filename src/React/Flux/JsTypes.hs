module React.Flux.JsTypes where

import Data.Aeson

#ifndef __GHCJS__
type JSRef a = ()
type Callback a = JSRef a
type Export a = JSRef a
#endif

-- | Represents the raw javascript event from React.
data RawEvent_

-- | The raw event received from React, and a decoded version of the event.
data RawEvent = RawEvent
    { _rawEventRef :: JSRef RawEvent_
    , _rawEventValue :: Value
    }

-- | This type is for the return value of React.createClass.
data ReactClass_
type ReactClassRef props = JSRef ReactClass_

data ReactElement_
type ReactElementRef = JSRef ReactElement_
