-- | This module contains the definitions for the
-- <https://facebook.github.io/react/docs/events.html React Event System>
module React.Flux.PropertiesAndEvents (
    PropertyOrHandler
  , (@=)
  , Event(..)
  , EventTarget(..)
  , eventTargetProp
  , target
  , preventDefault
  , stopPropagation

  -- * Keyboard
  , KeyboardEvent(..)
  , onKeyDown
  , onKeyPress
  , onKeyUp

  -- * Focus
  , FocusEvent(..)
  , onBlur
  , onFocus

  -- * Form
  , onChange
  , onInput
  , onSubmit

  -- * Mouse
  , MouseEvent(..)
  , onClick
  , onContextMenu
  , onDoubleClick
  , onDrag
  , onDragEnd
  , onDragEnter
  , onDragExit
  , onDragLeave
  , onDragOver
  , onDragStart
  , onDrop
  , onMouseDown
  , onMouseEnter
  , onMouseLeave
  , onMouseMove
  , onMouseOut
  , onMouseOver
  , onMouseUp

  -- * Touch
  , initializeTouchEvents
  , Touch(..)
  , TouchEvent(..)
  , onTouchCancel
  , onTouchEnd
  , onTouchMove
  , onTouchStart

  -- * UI
  , UIEvent(..)
  , onScroll

  -- * Wheel
  , WheelEvent(..)
  , onWheel

  -- * Creating your own handlers
  , on
  , HandlerArg(..)
  , parseEvent
  , parseKeyboardEvent
  , parseFocusEvent
  , parseMouseEvent
  , parseTouchEvent
  , parseWheelEvent
) where

import Control.Concurrent.MVar (newMVar)
import Data.String (IsString(..))
import Data.Aeson
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.Text as T

import React.Flux.Internal
import React.Flux.Store

#ifdef __GHCJS__
import GHCJS.Types (JSRef, nullRef)
import GHCJS.Marshal (fromJSRef)
import GHCJS.Marshal.Pure (PFromJSRef(..))
import qualified Data.JSString as JSString
import qualified JavaScript.Array as JSArray
#endif

-- | Create a property.
(@=) :: ToJSON a => T.Text -> a -> PropertyOrHandler handler
n @= a = Property (n, toJSON a)

----------------------------------------------------------------------------------------------------
--- Generic Event
----------------------------------------------------------------------------------------------------

-- | A reference to the object that dispatched the event.
-- <https://developer.mozilla.org/en-US/docs/Web/API/Event/target>
newtype EventTarget = EventTarget (JSRef ())

foreign import javascript unsafe
    "$r = $1[$2]"
    js_eventTargetProp :: EventTarget -> JSString.JSString -> JSRef val

-- | Use this to access a property of an event target.
eventTargetProp :: PFromJSRef val => EventTarget -> String -> val
eventTargetProp evt prop = pFromJSRef $ js_eventTargetProp evt (fromString prop)

-- | Every event in React is a synthetic event, a cross-browser wrapper around the native event.
data Event = Event
    { evtType :: String
    , evtBubbles :: Bool
    , evtCancelable :: Bool
    , evtCurrentTarget :: EventTarget
    , evtDefaultPrevented :: Bool
    , evtPhase :: Int
    , evtIsTrusted :: Bool
    -- evtNativeEvent
    , evtTarget :: EventTarget
    , evtTimestamp :: Int
    , evtHandlerArg :: HandlerArg
    }

-- | A version of 'eventTargetProp' which accesses the property of 'evtTarget' in the event.  This
-- is useful for example:
--
-- >div_ $
-- >    input_ [ "type" @= "checked"
-- >           , onChange $ \evt -> let val = target evt "value" in ...
-- >           ]
--
-- In this case, @val@ would coorespond to the javascript expression @evt.target.value@.
target :: PFromJSRef val => Event -> String -> val
target e s = eventTargetProp (evtTarget e) s

-- | In the FromJSON instance, we cannot fill in properties of an event which rely on the underlying
-- JSRef.  This newtype is here so that we do not export this FromJSON instance.  Instead, we export
-- parseEvent which correctly sets these properties.
newtype PartialEvent a = PartialEvent a

instance FromJSON (PartialEvent Event) where
    parseJSON = withObject "Event" $ \o -> PartialEvent <$> do
        Event <$> o .: "type"
              <*> o .: "bubbles"
              <*> o .: "cancelable"
              <*> pure undefined -- current target
              <*> o .: "defaultPrevented"
              <*> o .: "eventPhase"
              <*> o .: "isTrusted"
              <*> pure undefined -- target
              <*> o .: "timestamp"
              <*> pure undefined -- handler arg

foreign import javascript unsafe
    "$r = $1[$2]"
    js_GetEventRef :: JSRef () -> JSString.JSString -> JSRef ()

-- | Utility function to parse an 'Event' from the handler argument.
parseEvent :: HandlerArg -> Event
parseEvent arg@(HandlerArg ref val) =
    case fromJSON val of
        Error err -> error $ "Unable to parse event: " ++ err
        Success (PartialEvent e) -> e
            { evtCurrentTarget = EventTarget $ js_GetEventRef ref "currentTarget"
            , evtTarget = EventTarget $ js_GetEventRef ref "target"
            , evtHandlerArg = arg
            }

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
--- Clipboard
---------------------------------------------------------------------------------------------------


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

instance FromJSON (PartialEvent KeyboardEvent) where
    parseJSON = withObject "Keyboard Event" $ \o -> PartialEvent <$> do
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
        Success (PartialEvent e) -> e { keyGetModifierState = getModifierState ref }

onKeyDown :: (Event -> KeyboardEvent -> handler) -> PropertyOrHandler handler
onKeyDown = mkHandler "onKeyDown" parseKeyboardEvent

onKeyPress :: (Event -> KeyboardEvent -> handler) -> PropertyOrHandler handler
onKeyPress = mkHandler "onKeyPress" parseKeyboardEvent

onKeyUp :: (Event -> KeyboardEvent -> handler) -> PropertyOrHandler handler
onKeyUp = mkHandler "onKeyUp" parseKeyboardEvent

--------------------------------------------------------------------------------
-- Focus Events
--------------------------------------------------------------------------------

data FocusEvent = FocusEvent {
    focusRelatedTarget :: EventTarget
}

parseFocusEvent :: HandlerArg -> FocusEvent
parseFocusEvent (HandlerArg ref _) = FocusEvent $ EventTarget $ js_GetEventRef ref "relatedTarget"

onBlur :: (Event -> FocusEvent -> handler) -> PropertyOrHandler handler
onBlur = mkHandler "onBlur" parseFocusEvent

onFocus :: (Event -> FocusEvent -> handler) -> PropertyOrHandler handler
onFocus = mkHandler "onFocus" parseFocusEvent

--------------------------------------------------------------------------------
-- Form Events
--------------------------------------------------------------------------------

-- | The onChange event is special in React and should be used for all input change events.  For
-- details, see <https://facebook.github.io/react/docs/forms.html>
onChange :: (Event -> handler) -> PropertyOrHandler handler
onChange f = on "onChange" (f . parseEvent)

onInput :: (Event -> handler) -> PropertyOrHandler handler
onInput f = on "onInput" (f . parseEvent)

onSubmit :: (Event -> handler) -> PropertyOrHandler handler
onSubmit f = on "onSubmit" (f . parseEvent)

--------------------------------------------------------------------------------
-- Mouse Events
--------------------------------------------------------------------------------

data MouseEvent = MouseEvent
  { mouseAltKey :: Bool
  , mouseButton :: Int
  , mouseButtons :: Int
  , mouseClientX :: Int
  , mouseClientY :: Int
  , mouseCtrlKey :: Bool
  , mouseGetModifierState :: String -> Bool
  , mouseMetaKey :: Bool
  , mousePageX :: Int
  , mousePageY :: Int
  , mouseRelatedTarget :: EventTarget
  , mouseScreenX :: Int
  , mouseScreenY :: Int
  , mouseShiftKey :: Bool
  }

instance FromJSON (PartialEvent MouseEvent) where
    parseJSON = withObject "Mouse Event" $ \o -> PartialEvent <$> do
        MouseEvent <$> o .: "altKey"
                   <*> o .: "button"
                   <*> o .: "buttons"
                   <*> o .: "clientX"
                   <*> o .: "clientY"
                   <*> o .: "ctrlKey"
                   <*> return (pure False)
                   <*> o .: "metaKey"
                   <*> o .: "pageX"
                   <*> o .: "pageY"
                   <*> pure undefined -- related target
                   <*> o .: "screenX"
                   <*> o .: "screenY"
                   <*> o .: "shiftKey"

parseMouseEvent :: HandlerArg -> MouseEvent
parseMouseEvent (HandlerArg ref val) =
    case fromJSON val of
        Error err -> error $ "Unable to parse mouse event: " ++ err
        Success (PartialEvent e) -> e
            { mouseGetModifierState = getModifierState ref
            , mouseRelatedTarget = EventTarget $ js_GetEventRef ref "relatedTarget"
            }

onClick :: (Event -> MouseEvent -> handler) -> PropertyOrHandler handler
onClick = mkHandler "onClick" parseMouseEvent

onContextMenu :: (Event -> MouseEvent -> handler) -> PropertyOrHandler handler
onContextMenu = mkHandler "onContextMenu" parseMouseEvent

onDoubleClick :: (Event -> MouseEvent -> handler) -> PropertyOrHandler handler
onDoubleClick = mkHandler "onDoubleClick" parseMouseEvent

onDrag :: (Event -> MouseEvent -> handler) -> PropertyOrHandler handler
onDrag = mkHandler "onDrag" parseMouseEvent

onDragEnd :: (Event -> MouseEvent -> handler) -> PropertyOrHandler handler
onDragEnd = mkHandler "onDragEnd" parseMouseEvent

onDragEnter :: (Event -> MouseEvent -> handler) -> PropertyOrHandler handler
onDragEnter = mkHandler "onDragEnter" parseMouseEvent

onDragExit :: (Event -> MouseEvent -> handler) -> PropertyOrHandler handler
onDragExit = mkHandler "onDragExit" parseMouseEvent

onDragLeave :: (Event -> MouseEvent -> handler) -> PropertyOrHandler handler
onDragLeave = mkHandler "onDragLeave" parseMouseEvent

onDragOver :: (Event -> MouseEvent -> handler) -> PropertyOrHandler handler
onDragOver = mkHandler "onDragOver" parseMouseEvent

onDragStart :: (Event -> MouseEvent -> handler) -> PropertyOrHandler handler
onDragStart = mkHandler "onDragStart" parseMouseEvent

onDrop :: (Event -> MouseEvent -> handler) -> PropertyOrHandler handler
onDrop = mkHandler "onDrop" parseMouseEvent

onMouseDown :: (Event -> MouseEvent -> handler) -> PropertyOrHandler handler
onMouseDown = mkHandler "onMouseDown" parseMouseEvent

onMouseEnter :: (Event -> MouseEvent -> handler) -> PropertyOrHandler handler
onMouseEnter = mkHandler "onMouseEnter" parseMouseEvent

onMouseLeave :: (Event -> MouseEvent -> handler) -> PropertyOrHandler handler
onMouseLeave = mkHandler "onMouseLeave" parseMouseEvent

onMouseMove :: (Event -> MouseEvent -> handler) -> PropertyOrHandler handler
onMouseMove = mkHandler "onMouseMove" parseMouseEvent

onMouseOut :: (Event -> MouseEvent -> handler) -> PropertyOrHandler handler
onMouseOut = mkHandler "onMouseOut" parseMouseEvent

onMouseOver :: (Event -> MouseEvent -> handler) -> PropertyOrHandler handler
onMouseOver = mkHandler "onMouseOver" parseMouseEvent

onMouseUp :: (Event -> MouseEvent -> handler) -> PropertyOrHandler handler
onMouseUp = mkHandler "onMouseUp" parseMouseEvent

--------------------------------------------------------------------------------
-- Touch
--------------------------------------------------------------------------------

foreign import javascript unsafe
    "React.initializeTouchEvents(true)"
    initializeTouchEvents :: IO ()

data Touch = Touch {
    touchIdentifier :: Int
  , touchTarget :: EventTarget
  , touchScreenX :: Int
  , touchScreenY :: Int
  , touchClientX :: Int
  , touchClientY :: Int
  , touchPageX :: Int
  , touchPageY :: Int
}

instance FromJSON Touch where
    parseJSON = withObject "touch" $ \o ->
        Touch <$> o .: "identifier"
              <*> pure undefined
              <*> o .: "screenX"
              <*> o .: "screenY"
              <*> o .: "clientX"
              <*> o .: "clientY"
              <*> o .: "pageX"
              <*> o .: "pageY"

data TouchEvent = TouchEvent {
    touchAltKey :: Bool
  , changedTouches :: [Touch]
  , touchCtrlKey :: Bool
  , touchGetModifierState :: String -> Bool
  , touchMetaKey :: Bool
  , touchShiftKey :: Bool
  , touchTargets :: [Touch]
  , touches :: [Touch]
  }

foreign import javascript unsafe
    "$r = $1[$2]"
    js_GetTouchList :: JSRef () -> JSString.JSString -> JSArray.JSArray

parseTouchList :: JSRef () -> JSString.JSString -> [Touch]
parseTouchList obj key = map f [0..(JSArray.length arr - 1)]
    where
        arr = js_GetTouchList obj key
        f idx = let jsref = JSArray.index idx arr
                    val = unsafePerformIO $ maybe (error "Unable to parse touch") return =<< fromJSRef jsref
                  in case fromJSON val of
                        Error err -> error $ "Unable to parse touch: " ++ err
                        Success s -> s { touchTarget = EventTarget $ js_GetEventRef obj "target" }

instance FromJSON (PartialEvent TouchEvent) where
    parseJSON = withObject "touch event" $ \o -> PartialEvent <$> do
        TouchEvent <$> o .: "altKey"
                   <*> pure []
                   <*> o .: "ctrlKey"
                   <*> return (pure False)
                   <*> o .: "metaKey"
                   <*> o .: "shiftKey"
                   <*> pure []
                   <*> pure []

parseTouchEvent :: HandlerArg -> TouchEvent
parseTouchEvent (HandlerArg ref val) =
    case fromJSON val of
        Error err -> error $ "Unable to parse touch event: " ++ err
        Success (PartialEvent e) -> e
            { changedTouches = parseTouchList ref "changedTouches"
            , touchGetModifierState = getModifierState ref
            , touchTargets = parseTouchList ref "targets"
            , touches = parseTouchList ref "targets"
            }

onTouchCancel :: (Event -> TouchEvent -> handler) -> PropertyOrHandler handler
onTouchCancel = mkHandler "onTouchCancel" parseTouchEvent

onTouchEnd :: (Event -> TouchEvent -> handler) -> PropertyOrHandler handler
onTouchEnd = mkHandler "onTouchEnd" parseTouchEvent

onTouchMove :: (Event -> TouchEvent -> handler) -> PropertyOrHandler handler
onTouchMove = mkHandler "onTouchMove" parseTouchEvent

onTouchStart :: (Event -> TouchEvent -> handler) -> PropertyOrHandler handler
onTouchStart = mkHandler "onTouchStart" parseTouchEvent

--------------------------------------------------------------------------------
-- UI Events
--------------------------------------------------------------------------------

data UIEvent = UIEvent {
    uiDetail :: Int
  -- abstract view
}

instance FromJSON UIEvent where
    parseJSON = withObject "ui event" $ \o -> UIEvent <$> o .: "detail"

parseUIEvent :: HandlerArg -> UIEvent
parseUIEvent (HandlerArg _ val) =
    case fromJSON val of
        Error err -> error $ "Unable to parse ui event: " ++ err
        Success e -> e

onScroll :: (Event -> UIEvent -> handler) -> PropertyOrHandler handler
onScroll = mkHandler "onScroll" parseUIEvent

--------------------------------------------------------------------------------
-- Wheel
--------------------------------------------------------------------------------

data WheelEvent = WheelEvent {
    wheelDeltaMode :: Int
  , wheelDeltaX :: Int
  , wheelDeltaY :: Int
  , wheelDeltaZ :: Int
}

instance FromJSON WheelEvent where
    parseJSON = withObject "wheel event" $ \o ->
        WheelEvent <$> o .: "deltaMode"
                   <*> o .: "deltaX"
                   <*> o .: "deltaY"
                   <*> o .: "deltaZ"

parseWheelEvent :: HandlerArg -> WheelEvent
parseWheelEvent (HandlerArg _ val) =
    case fromJSON val of
        Error err -> error $ "Unable to parse wheel event: " ++ err
        Success e -> e

onWheel :: (Event -> WheelEvent -> handler) -> PropertyOrHandler handler
onWheel = mkHandler "onWheel" parseWheelEvent
