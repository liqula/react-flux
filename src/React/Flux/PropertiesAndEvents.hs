-- | This module contains the definitions for the
-- <https://facebook.github.io/react/docs/events.html React Event System>
module React.Flux.PropertiesAndEvents (
    PropertyOrHandler
  , (@=)
  , ($=)
  , callback

  -- * Events
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
) where

import Control.Concurrent.MVar (newMVar)
import Control.DeepSeq
import Control.Monad (forM)
import Data.Maybe (fromMaybe)
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.Text as T
import qualified Data.Aeson as A

import React.Flux.Internal
import React.Flux.Store

#ifdef __GHCJS__
import GHCJS.Types (JSRef, nullRef, JSString, JSBool)
import GHCJS.Foreign (toJSString, lengthArray, indexArray, fromJSBool)
import GHCJS.Marshal (FromJSRef(..))
#endif

-- | Create a property.
(@=) :: A.ToJSON a => T.Text -> a -> PropertyOrHandler handler
n @= a = Property (n, A.toJSON a)

-- | Create a text-valued property.  This is here to avoid problems when OverloadedStrings extension
-- is enabled
($=) :: T.Text -> T.Text -> PropertyOrHandler handler
n $= a = Property (n, A.toJSON a)

-- | Create a callback property.
callback :: String -> (A.Value -> handler) -> PropertyOrHandler handler
callback = CallbackProperty

----------------------------------------------------------------------------------------------------
--- Generic Event
----------------------------------------------------------------------------------------------------

-- | A reference to the object that dispatched the event.
-- <https://developer.mozilla.org/en-US/docs/Web/API/Event/target>
newtype EventTarget = EventTarget (JSRef ())

instance Show (EventTarget) where
    show _ = "EventTarget"

foreign import javascript unsafe
    "$1[$2]"
    js_getProp :: JSRef a -> JSString -> JSRef b

-- | Access a property from an object.  Since event objects are immutable, we can use
-- unsafePerformIO without worry.
(.:) :: FromJSRef b => JSRef a -> JSString -> b
obj .: key = fromMaybe (error "Unable to decode event target") $ unsafePerformIO $
    fromJSRef $ js_getProp obj key

-- | Access a property in an event target
eventTargetProp :: FromJSRef val => EventTarget -> String -> val
eventTargetProp (EventTarget ref) key = ref .: toJSString key

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
    } deriving (Show)

-- | A version of 'eventTargetProp' which accesses the property of 'evtTarget' in the event.  This
-- is useful for example:
--
-- >div_ $
-- >    input_ [ "type" @= "checked"
-- >           , onChange $ \evt -> let val = target evt "value" in ...
-- >           ]
--
-- In this case, @val@ would coorespond to the javascript expression @evt.target.value@.
target :: FromJSRef val => Event -> String -> val
target e s = eventTargetProp (evtTarget e) s

parseEvent :: HandlerArg -> Event
parseEvent arg@(HandlerArg o) = Event
    { evtType = o .: "type"
    , evtBubbles = o .: "bubbles"
    , evtCancelable = o .: "cancelable"
    , evtCurrentTarget = EventTarget $ js_getProp o "currentTarget"
    , evtDefaultPrevented = o .: "defaultPrevented"
    , evtPhase = o .: "eventPhase"
    , evtIsTrusted = o .: "isTrusted"
    , evtTarget = EventTarget $ js_getProp o "target"
    , evtTimestamp = o .: "timeStamp"
    , evtHandlerArg = arg
    }

-- | Create an event handler from a name and a handler function.
--
-- This can also be used to pass in arbitrary callbacks to foreign javascript React classes.
-- Indeed, what 'on' does is create a callback and then add a property with key the string passed to
-- 'on' and value the callback.
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

instance StoreData FakeEventStoreData where
    type StoreAction FakeEventStoreData = FakeEventStoreAction
    transform _ _ = return FakeEventStoreData

#ifdef __GHCJS__

-- | What a hack!  React re-uses event objects in a pool.  To make sure this is OK, we must perform
-- all computation involving the event object before it is returned to React.  But the callback
-- registered in the handler will return anytime the Haskell thread blocks, and the Haskell thread
-- will continue asynchronously.  If this occurs, the event object is no longer valid.  Thus, inside
-- the event handlers in Views.hs, the handler will use 'deepseq' to force all the actions before
-- starting any of the transforms (which could block).  We rely on this call plus use
-- unsafePerformIO to call the appropriate functions on the event object.
instance NFData FakeEventStoreAction where
    rnf (PreventDefault (HandlerArg ref)) = unsafePerformIO (js_preventDefault ref) `deepseq` ()
    rnf (StopPropagation (HandlerArg ref)) = unsafePerformIO (js_stopProp ref) `deepseq` ()

foreign import javascript unsafe
    "$1['preventDefault']();"
    js_preventDefault :: JSRef () -> IO ()

foreign import javascript unsafe
    "$1['stopPropagation']();"
    js_stopProp :: JSRef () -> IO ()

#else

instance NFData FakeEventStoreAction where
    rnf _ = ()

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

instance Show KeyboardEvent where
    show (KeyboardEvent k1 k2 k3 _ k4 k5 k6 k7 k8 k9 k10 k11) =
        show (k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11)

#ifdef __GHCJS__
foreign import javascript unsafe
    "$1['getModifierState']($2)"
    js_GetModifierState :: JSRef () -> JSString -> JSBool

getModifierState :: JSRef () -> String -> Bool
getModifierState ref = fromJSBool . js_GetModifierState ref . toJSString
#else
getModifierState :: JSRef () -> String -> Bool
getModifierState _ _ = False
#endif

parseKeyboardEvent :: HandlerArg -> KeyboardEvent
parseKeyboardEvent (HandlerArg o) = KeyboardEvent
    { keyEvtAltKey = o .: "altKey"
    , keyEvtCharCode = o .: "charCode"
    , keyEvtCtrlKey = o .: "ctrlKey"
    , keyGetModifierState = getModifierState o
    , keyKey = o .: "key"
    , keyCode = o .: "keyCode"
    , keyLocale = o .: "locale"
    , keyLocation = o .: "location"
    , keyMetaKey = o .: "metaKey"
    , keyRepeat = o .: "repeat"
    , keyShiftKey = o .: "shiftKey"
    , keyWhich = o .: "which"
    }

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
} deriving (Show)

parseFocusEvent :: HandlerArg -> FocusEvent
parseFocusEvent (HandlerArg ref) = FocusEvent $ EventTarget $ js_getProp ref "relatedTarget"

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

instance Show MouseEvent where
    show (MouseEvent m1 m2 m3 m4 m5 m6 _ m7 m8 m9 m10 m11 m12 m13)
        = show (m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11, m12, m13)

parseMouseEvent :: HandlerArg -> MouseEvent
parseMouseEvent (HandlerArg o) = MouseEvent
    { mouseAltKey = o .: "altKey"
    , mouseButton = o .: "button"
    , mouseButtons = o .: "buttons"
    , mouseClientX = o .: "clientX"
    , mouseClientY = o .: "clientY"
    , mouseCtrlKey = o .: "ctrlKey"
    , mouseGetModifierState = getModifierState o
    , mouseMetaKey = o .: "metaKey"
    , mousePageX = o .: "pageX"
    , mousePageY = o .: "pageY"
    , mouseRelatedTarget = EventTarget $ js_getProp o "relatedTarget"
    , mouseScreenX = o .: "screenX"
    , mouseScreenY = o .: "screenY"
    , mouseShiftKey = o .: "shiftKey"
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
    "React['initializeTouchEvents'](true)"
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
} deriving (Show)

parseTouch :: JSRef a -> Touch
parseTouch o = Touch
    { touchIdentifier = o .: "identifier"
    , touchTarget = EventTarget $ js_getProp o "target"
    , touchScreenX = o .: "screenX"
    , touchScreenY = o .: "screenY"
    , touchClientX = o .: "clientX"
    , touchClientY = o .: "clientY"
    , touchPageX = o .: "pageX"
    , touchPageY = o .: "pageY"
    }

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

instance Show TouchEvent where
    show (TouchEvent t1 t2 t3 _ t4 t5 t6 t7)
        = show (t1, t2, t3, t4, t5, t6, t7)

parseTouchList :: JSRef a -> JSString -> [Touch]
parseTouchList obj key = unsafePerformIO $ do
    let arr = js_getProp obj key
    len <- lengthArray arr
    forM [0..len-1] $ \idx -> do
        jsref <- indexArray idx arr
        return $ parseTouch jsref

parseTouchEvent :: HandlerArg -> TouchEvent
parseTouchEvent (HandlerArg o) = TouchEvent
    { touchAltKey = o .: "altKey"
    , changedTouches = parseTouchList o "changedTouches"
    , touchCtrlKey = o .: "ctrlKey"
    , touchGetModifierState = getModifierState o
    , touchMetaKey = o .: "metaKey"
    , touchShiftKey = o .: "shiftKey"
    , touchTargets = parseTouchList o "targetTouches"
    , touches = parseTouchList o "touches"
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
} deriving (Show)

parseUIEvent :: HandlerArg -> UIEvent
parseUIEvent (HandlerArg o) = UIEvent
    { uiDetail = o .: "detail"
    }

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
} deriving (Show)

parseWheelEvent :: HandlerArg -> WheelEvent
parseWheelEvent (HandlerArg o) = WheelEvent
    { wheelDeltaMode = o .: "deltaMode"
    , wheelDeltaX = o .: "deltaX"
    , wheelDeltaY = o .: "deltaY"
    , wheelDeltaZ = o .: "deltaZ"
    }

onWheel :: (Event -> WheelEvent -> handler) -> PropertyOrHandler handler
onWheel = mkHandler "onWheel" parseWheelEvent
