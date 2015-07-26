-- | This module contains the definitions for the
-- <https://facebook.github.io/react/docs/events.html React Event System>
module React.Flux.PropertiesAndEvents (
    PropertyOrHandler
  , (@=)
  , Event(..)

  -- * Keyboard
  , KeyboardEvent(..)
  , onKeyDown
  , onKeyPress
  , onKeyUp

  -- * Creating your own handlers
  , mkHandler
  , RawEvent_
  , RawEvent(..)
  , parseEvent
) where

import Data.Aeson
import Data.Aeson.Types (Pair)
import React.Flux.JsTypes
import qualified Data.Text as T

-- | An event handler.  Values of this type are created by the various functions below such as
-- 'onKeyDown'.  The handler is then passed to the element creation functions in "React.Flux.Dom".
data PropertyOrHandler handler =
   Property Pair
 | EventHandler
      { evtHandlerName :: String
      , evtHandler :: RawEvent -> handler
      }

instance Functor PropertyOrHandler where
    fmap _ (Property p) = Property p
    fmap f (EventHandler name h) = EventHandler name (f . h)

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
    }

instance FromJSON Event where
    parseJSON = withObject "Event" $ \o -> do
        Event <$> o .: "type"
              <*> o .: "bubbles"
              <*> o .: "cancelable"
              <*> o .: "defaultPrevented"
              <*> o .: "eventPhase"
              <*> o .: "isTrusted"
              <*> o .: "timestamp"

{-
foreign import javascript unsafe
    "$1.preventDefault();"
    js_preventDefault :: JSRef RawEvent_ -> IO ()

preventDefault :: RawEvent -> IO ()
preventDefault (RawEvent ref _) = js_preventDefault ref

foreign import javascript unsafe
    "$1.stopPropagation();"
    js_stopProp :: JSRef RawEvent_ -> IO ()

stopPropagation :: RawEvent -> IO ()
stopPropagation (RawEvent ref _) = js_stopProp ref
-}

parseEvent :: RawEvent -> Event
parseEvent (RawEvent _ val) =
    case fromJSON val of
        Error err -> error $ "Unable to parse event: " ++ err
        Success e -> e

-- | Construct an 'EventHandler' 
mkHandler :: String -- ^ The event name
          -> (RawEvent -> detail) -- ^ A function parsing the details for the specific event.
          -> (Event -> detail -> handler) -- ^ The function implementing the handler.
          -> PropertyOrHandler handler
mkHandler name parseDetail f = EventHandler
    { evtHandlerName = name
    , evtHandler = \raw -> f (parseEvent raw) (parseDetail raw)
    }

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
    "$r = $1.getModifierState($2)"
    js_GetModifierState :: JSRef RawEvent_ -> JSString -> JSBool

getModifierState :: JSRef RawEvent_ -> String -> Bool
getModifierState ref = pFromJSRef . js_GetModifierState . pToJSRef
#else
getModifierState :: JSRef RawEvent_ -> String -> Bool
getModifierState _ _ = False
#endif

parseKeyboardEvent :: RawEvent -> KeyboardEvent
parseKeyboardEvent (RawEvent ref val) =
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
