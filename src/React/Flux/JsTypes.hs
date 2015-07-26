module React.Flux.JsTypes where

import Data.Aeson

#ifndef __GHCJS__
type JSRef a = ()
type Callback a = JSRef a
#endif

data RawEvent_
-- | A raw event from React
data RawEvent = RawEvent
    { _rawEventRef :: JSRef RawEvent_
    , _rawEventValue :: Value
    }

-- | This type is for the return value of React.createClass.
data ReactClass_
type ReactClassRef props = JSRef ReactClass_

data ReactElement_
type ReactElementRef = JSRef ReactElement_
