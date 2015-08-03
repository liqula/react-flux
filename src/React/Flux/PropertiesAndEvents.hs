-- | This module contains the definitions for the
-- <https://facebook.github.io/react/docs/events.html React Event System>
module React.Flux.PropertiesAndEvents (
    PropertyOrHandler
  , (@=)
  , Event(..)
  , preventDefault
  , stopPropagation

  -- * Keyboard
  , KeyboardEvent(..)
  , onKeyDown
  , onKeyPress
  , onKeyUp

  -- * Creating your own handlers
  , on
  , HandlerArg(..)
  , parseEvent
) where

import Control.Concurrent.MVar (newMVar)
import Data.Aeson
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.Text as T

import React.Flux.Internal
import React.Flux.Store

#ifdef __GHCJS__
import GHCJS.Types (JSRef, nullRef)
import GHCJS.Marshal.Pure (pFromJSRef)
import qualified Data.JSString as JSString
#endif

-- | Create a property.
(@=) :: ToJSON a => T.Text -> a -> PropertyOrHandler handler
n @= a = Property (n, toJSON a)

----------------------------------------------------------------------------------------------------
--- Generic Event
----------------------------------------------------------------------------------------------------

-- | Every event in React is a synthetic event, a cross-browser wrapper around the native event.
data Event = Event
    { evtType :: String
    , evtBubbles :: Bool
    , evtCancelable :: Bool
    -- evtCurrentTarget
    , evtDefaultPrevented :: Bool
    , evtPhase :: Int
    , evtIsTrusted :: Bool
    -- evtNativeEvent
    -- evtTarget
    , evtTimestamp :: Int
    , evtHandlerArg :: HandlerArg
    }

-- | In the FromJSON instance, we cannot fill in evtHandlerArg so it must be undefined.
-- This newtype is here so that we do not export this FromJSON instance.  Instead, we export
-- parseEvent which correctly sets evtHandlerArg.
newtype EventWithUndefinedHandlerArg = EvtNoHArg Event

instance FromJSON EventWithUndefinedHandlerArg where
    parseJSON = withObject "Event" $ \o -> EvtNoHArg <$> do
        Event <$> o .: "type"
              <*> o .: "bubbles"
              <*> o .: "cancelable"
              <*> o .: "defaultPrevented"
              <*> o .: "eventPhase"
              <*> o .: "isTrusted"
              <*> o .: "timestamp"
              <*> undefined

-- | Utility function to parse an 'Event' from the handler argument.
parseEvent :: HandlerArg -> Event
parseEvent arg@(HandlerArg _ val) =
    case fromJSON val of
        Error err -> error $ "Unable to parse event: " ++ err
        Success (EvtNoHArg e) -> e { evtHandlerArg = arg }

-- | Create an event handler from a name and a handler function.
on :: String -> (HandlerArg -> handler) -> PropertyOrHandler handler
on = EventHandler

-- | Construct a handler from a detail parser, used by the various events below.
mkHandler :: String -- ^ The event name
          -> (HandlerArg -> detail) -- ^ A function parsing the details for the specific event.
          -> (Event -> detail -> handler) -- ^ The function implementing the handler.
          -> PropertyOrHandler handler
mkHandler name parseDetail f = EventHandler
    { evtHandlerName = name
    , evtHandler = \raw -> f (parseEvent raw) (parseDetail raw)
    }


-- | In a hack, the prevent default and stop propagation are actions since that is the easiest way
-- of allowing users to specify these actions (IO is not available in view event handlers).  We
-- create a fake store to handle these actions.
data FakeEventStoreData = FakeEventStoreData

-- | The fake store, doesn't store any data.  Also, the dispatch function correctly detects
-- nullRef and will not attempt to notify any controller-views.
fakeEventStore :: ReactStore FakeEventStoreData
fakeEventStore = unsafePerformIO (ReactStore (ReactStoreRef nullRef) <$> newMVar FakeEventStoreData)
{-# NOINLINE fakeEventStore #-}

-- | The actions for the fake store
data FakeEventStoreAction = PreventDefault HandlerArg
                          | StopPropagation HandlerArg

#ifdef __GHCJS__

instance StoreData FakeEventStoreData where
    type StoreAction FakeEventStoreData = FakeEventStoreAction
    transform (PreventDefault (HandlerArg ref _)) _ = js_preventDefault ref >> return FakeEventStoreData
    transform (StopPropagation (HandlerArg ref _)) _ = js_stopProp ref >> return FakeEventStoreData

foreign import javascript unsafe
    "$1.preventDefault();"
    js_preventDefault :: JSRef () -> IO ()

foreign import javascript unsafe
    "$1.stopPropagation();"
    js_stopProp :: JSRef () -> IO ()

#else

instance StoreData FakeEventStoreData where
    type StoreAction FakeEventStoreData = FakeEventStoreAction
    transform _ _ = return FakeEventStoreAction

#endif

-- | Prevent the default browser action from occuring in response to this event.
preventDefault :: Event -> SomeStoreAction
preventDefault = SomeStoreAction fakeEventStore . PreventDefault . evtHandlerArg

-- | Stop propagating this event up the DOM tree.
stopPropagation :: Event -> SomeStoreAction
stopPropagation = SomeStoreAction fakeEventStore . StopPropagation . evtHandlerArg

---------------------------------------------------------------------------------------------------
--- Keyboard
---------------------------------------------------------------------------------------------------

-- | The data for the keyboard events
data KeyboardEvent = KeyboardEvent
  { keyEvtAltKey :: Bool
  , keyEvtCharCode :: Int
  , keyEvtCtrlKey :: Bool
  , keyGetModifierState :: String -> Bool
  , keyKey :: String
  , keyCode :: Int
  , keyLocale :: String
  , keyLocation :: Int
  , keyMetaKey :: Bool
  , keyRepeat :: Bool
  , keyShiftKey :: Bool
  , keyWhich :: Int
  }

instance FromJSON KeyboardEvent where
    parseJSON = withObject "Keyboard Event" $ \o ->
        KeyboardEvent <$> o .: "altKey"
                      <*> o .: "charCode"
                      <*> o .: "ctrlKey"
                      <*> return (pure False) -- this is set in 'parseKeyboardEvent'
                      <*> o .: "key"
                      <*> o .: "keyCode"
                      <*> o .: "locale"
                      <*> o .: "location"
                      <*> o .: "metaKey"
                      <*> o .: "repeat"
                      <*> o .: "shiftKey"
                      <*> o .: "which"

#ifdef __GHCJS__
foreign import javascript unsafe
    "$1.getModifierState($2)"
    js_GetModifierState :: JSRef () -> JSString.JSString -> JSRef Bool

getModifierState :: JSRef () -> String -> Bool
getModifierState ref = pFromJSRef . js_GetModifierState ref . JSString.pack
#else
getModifierState :: JSRef () -> String -> Bool
getModifierState _ _ = False
#endif

parseKeyboardEvent :: HandlerArg -> KeyboardEvent
parseKeyboardEvent (HandlerArg ref val) =
    case fromJSON val of
        Error err -> error $ "Unable to parse keyboard event: " ++ err
        Success e -> e
                { keyGetModifierState = getModifierState ref
                }

onKeyDown :: (Event -> KeyboardEvent -> handler) -> PropertyOrHandler handler
onKeyDown = mkHandler "onKeyDown" parseKeyboardEvent

onKeyPress :: (Event -> KeyboardEvent -> handler) -> PropertyOrHandler handler
onKeyPress = mkHandler "onKeyPress" parseKeyboardEvent

onKeyUp :: (Event -> KeyboardEvent -> handler) -> PropertyOrHandler handler
onKeyUp = mkHandler "onKeyUp" parseKeyboardEvent
