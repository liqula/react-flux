-- | This module contains the definitions for creating properties to pass to javascript elements and
-- foreign javascript classes.  In addition, it contains definitions for the
-- <https://facebook.github.io/react/docs/events.html React Event System>.
{-# LANGUAGE UndecidableInstances #-}
module React.Flux.PropertiesAndEvents (
    PropertyOrHandler

  -- * Creating Properties
  , property
  , elementProperty
  , nestedProperty
  , CallbackFunction
  , callback

  -- ** Combinators
  , (@=)
  , ($=)
  , classNames

  -- * Creating Events
  , Event(..)
  , EventTarget(..)
  , eventTargetProp
  , target
  , preventDefault
  , stopPropagation
  , capturePhase
  , on

  -- ** Keyboard
  , KeyboardEvent(..)
  , onKeyDown
  , onKeyPress
  , onKeyUp

  -- ** Focus
  , FocusEvent(..)
  , onBlur
  , onFocus

  -- ** Form
  , onChange
  , onInput
  , onSubmit

  -- ** Mouse
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

  -- ** Touch
  , initializeTouchEvents
  , Touch(..)
  , TouchEvent(..)
  , onTouchCancel
  , onTouchEnd
  , onTouchMove
  , onTouchStart

  -- ** UI
  , onScroll

  -- ** Wheel
  , WheelEvent(..)
  , onWheel

  -- ** Image
  , onLoad
  , onError
) where

import           Control.Monad (forM)
import           Control.Concurrent.MVar (newMVar)
import           Control.DeepSeq
import           System.IO.Unsafe (unsafePerformIO)
import qualified Data.Text as T
import qualified Data.Aeson as A
import qualified Data.HashMap.Strict as M

import           React.Flux.Internal
import           React.Flux.Store
import           React.Flux.Views (ViewEventHandler, StatefulViewEventHandler)

#ifdef __GHCJS__
import           Data.Maybe (fromMaybe)

import           GHCJS.Foreign (fromJSBool)
import           GHCJS.Marshal (FromJSRef(..))
import           GHCJS.Types (JSRef, nullRef, JSString, IsJSRef)
import           JavaScript.Array as JSA

#else
type JSRef = ()
type JSString = String
type JSArray = ()
class FromJSRef a
class IsJSRef a
nullRef :: ()
nullRef = ()
#endif

-- | Some third-party React classes allow passing React elements as properties.  This function
-- will first run the given 'ReactElementM' to obtain an element or elements, and then use that
-- element as the value for a property with the given key.
elementProperty :: String -> ReactElementM handler () -> PropertyOrHandler handler
elementProperty = ElementProperty

-- | Allows you to create nested object properties.  The list of properties passed in will be
-- converted to an object which is then set as the value for a property with the given name.  For
-- example,
--
-- >[ nestedProperty "Hello" [ "a" @= (100 :: Int), "b" $= "World" ]
-- >, "c" $= "!!!"
-- >]
--
-- would create a javascript object
--
-- >{"Hello": {a: 100, b: "World"}, "c": "!!!"}
nestedProperty :: String -> [PropertyOrHandler handler] -> PropertyOrHandler handler
nestedProperty = NestedProperty

-- | A class which is used to implement <https://wiki.haskell.org/Varargs variable argument functions>.
-- Any function where each argument implements 'FromJSRef' and the result is either
-- 'ViewEventHandler' or 'StatefulViewEventHandler' is an instance of this class.
class CallbackFunction handler a | a -> handler  where
    applyFromArguments :: JSArray -> Int -> a -> IO handler

instance CallbackFunction ViewEventHandler ViewEventHandler where
    applyFromArguments _ _ h = return h

instance CallbackFunction (StatefulViewEventHandler s) (StatefulViewEventHandler s) where
    applyFromArguments _ _ h = return h

instance (FromJSRef a, CallbackFunction handler b) => CallbackFunction handler (a -> b) where
#if __GHCJS__
    applyFromArguments args k f = do
        ma <- fromJSRef $ if k >= JSA.length args then nullRef else JSA.index k args
        a <- maybe (error "Unable to decode callback argument") return ma
        applyFromArguments args (k+1) $ f a
#else
    applyFromArguments _ _ _ = error "Not supported in GHC"
#endif

-- | Create a callback property.  This is primarily intended for foreign React classes which expect
-- callbacks to be passed to them as properties.  For events on DOM elements, you should instead use
-- the handlers below.
--
-- The function @func@ can be any function, as long as each argument to the function is an instance
-- of 'FromJSRef' and the result of the function is @handler@.  Internally, 'callback' creates a
-- javascript function which accesses the @arguments@ javascript object and then matches entries in
-- @arguments@ to the parameters of @func@.  If @func@ has more parameters than the javascript
-- @arguments@ object, a javascript null is used for the conversion.  Since the 'Maybe' instance of
-- 'FromJSRef' converts a null reference to 'Nothing', you can exploit this to create
-- variable-argument javascript callbacks.
--
-- For example, all three of the following functions could be passed as @func@ inside a view.
--
-- >foo :: Int -> Maybe String -> ViewEventHandler
-- >bar :: Aeson.Value -> ViewEventHandler
-- >baz :: ViewEventHandler
--
-- For another example, see the haddock comments in "React.Flux.Addons.Bootstrap".
callback :: CallbackFunction handler func => String -> func -> PropertyOrHandler handler
callback name func = CallbackPropertyWithArgumentArray name $ \arr -> applyFromArguments arr 0 func

----------------------------------------------------------------------------------------------------
--- Combinators
----------------------------------------------------------------------------------------------------

-- | Create a property.
(@=) :: A.ToJSON a => T.Text -> a -> PropertyOrHandler handler
n @= a = Property (T.unpack n) (A.toJSON a)

-- | Create a text-valued property.  This is here to avoid problems when OverloadedStrings extension
-- is enabled
($=) :: T.Text -> T.Text -> PropertyOrHandler handler
n $= a = Property (T.unpack n) a

-- | Set the <https://facebook.github.io/react/docs/class-name-manipulation.html className> property to consist
-- of all the names which are matched with True, allowing you to easily toggle class names based on
-- a computation.
classNames :: [(T.Text, Bool)] -> PropertyOrHandler handler
classNames xs = "className" @= T.intercalate " " names
    where
        names = M.keys $ M.filter id $ M.fromList xs

----------------------------------------------------------------------------------------------------
--- Generic Event
----------------------------------------------------------------------------------------------------

-- | A reference to the object that dispatched the event.
-- <https://developer.mozilla.org/en-US/docs/Web/API/Event/target>
newtype EventTarget = EventTarget JSRef
instance IsJSRef EventTarget

instance Show (EventTarget) where
    show _ = "EventTarget"

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

-- | Use this to create an event handler for an event not covered by the rest of this module.  At
-- the moment, this is just the media events (onPlay, onPause, etc.) on image and video tags.
on :: String -> (Event -> handler) -> PropertyOrHandler handler
on name f = CallbackPropertyWithSingleArgument
    { csPropertyName = name
    , csFunc = f . parseEvent
    }

-- | Construct a handler from a detail parser, used by the various events below.
on2 :: String -- ^ The event name
    -> (HandlerArg -> detail) -- ^ A function parsing the details for the specific event.
    -> (Event -> detail -> handler) -- ^ The function implementing the handler.
    -> PropertyOrHandler handler
on2 name parseDetail f = CallbackPropertyWithSingleArgument
    { csPropertyName = name
    , csFunc = \raw -> f (parseEvent raw) (parseDetail raw)
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
    js_preventDefault :: JSRef -> IO ()

foreign import javascript unsafe
    "$1['stopPropagation']();"
    js_stopProp :: JSRef -> IO ()

#else

instance NFData FakeEventStoreAction where
    rnf _ = ()

#endif

-- | Prevent the default browser action from occuring in response to this event.
preventDefault :: Event -> SomeStoreAction
preventDefault = SomeStoreAction fakeEventStore . PreventDefault . evtHandlerArg

-- | Stop propagating this event, either down the DOM tree during the capture phase or up the DOM
-- tree during the bubbling phase.
stopPropagation :: Event -> SomeStoreAction
stopPropagation = SomeStoreAction fakeEventStore . StopPropagation . evtHandlerArg

-- | By default, the handlers below are triggered during the bubbling phase.  Use this to switch
-- them to trigger during the capture phase.
capturePhase :: PropertyOrHandler handler -> PropertyOrHandler handler
capturePhase (CallbackPropertyWithSingleArgument n h) = CallbackPropertyWithSingleArgument (n ++ "Capture") h
capturePhase _ = error "You must use React.Flux.PropertiesAndEvents.capturePhase on an event handler"

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
onKeyDown = on2 "onKeyDown" parseKeyboardEvent

onKeyPress :: (Event -> KeyboardEvent -> handler) -> PropertyOrHandler handler
onKeyPress = on2 "onKeyPress" parseKeyboardEvent

onKeyUp :: (Event -> KeyboardEvent -> handler) -> PropertyOrHandler handler
onKeyUp = on2 "onKeyUp" parseKeyboardEvent

--------------------------------------------------------------------------------
-- Focus Events
--------------------------------------------------------------------------------

data FocusEvent = FocusEvent {
    focusRelatedTarget :: EventTarget
} deriving (Show)

parseFocusEvent :: HandlerArg -> FocusEvent
parseFocusEvent (HandlerArg ref) = FocusEvent $ EventTarget $ js_getProp ref "relatedTarget"

onBlur :: (Event -> FocusEvent -> handler) -> PropertyOrHandler handler
onBlur = on2 "onBlur" parseFocusEvent

onFocus :: (Event -> FocusEvent -> handler) -> PropertyOrHandler handler
onFocus = on2 "onFocus" parseFocusEvent

--------------------------------------------------------------------------------
-- Form Events
--------------------------------------------------------------------------------

-- | The onChange event is special in React and should be used for all input change events.  For
-- details, see <https://facebook.github.io/react/docs/forms.html>
onChange :: (Event -> handler) -> PropertyOrHandler handler
onChange = on "onChange"

onInput :: (Event -> handler) -> PropertyOrHandler handler
onInput = on "onInput"

onSubmit :: (Event -> handler) -> PropertyOrHandler handler
onSubmit = on "onSubmit"

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
onClick = on2 "onClick" parseMouseEvent

onContextMenu :: (Event -> MouseEvent -> handler) -> PropertyOrHandler handler
onContextMenu = on2 "onContextMenu" parseMouseEvent

onDoubleClick :: (Event -> MouseEvent -> handler) -> PropertyOrHandler handler
onDoubleClick = on2 "onDoubleClick" parseMouseEvent

onDrag :: (Event -> MouseEvent -> handler) -> PropertyOrHandler handler
onDrag = on2 "onDrag" parseMouseEvent

onDragEnd :: (Event -> MouseEvent -> handler) -> PropertyOrHandler handler
onDragEnd = on2 "onDragEnd" parseMouseEvent

onDragEnter :: (Event -> MouseEvent -> handler) -> PropertyOrHandler handler
onDragEnter = on2 "onDragEnter" parseMouseEvent

onDragExit :: (Event -> MouseEvent -> handler) -> PropertyOrHandler handler
onDragExit = on2 "onDragExit" parseMouseEvent

onDragLeave :: (Event -> MouseEvent -> handler) -> PropertyOrHandler handler
onDragLeave = on2 "onDragLeave" parseMouseEvent

onDragOver :: (Event -> MouseEvent -> handler) -> PropertyOrHandler handler
onDragOver = on2 "onDragOver" parseMouseEvent

onDragStart :: (Event -> MouseEvent -> handler) -> PropertyOrHandler handler
onDragStart = on2 "onDragStart" parseMouseEvent

onDrop :: (Event -> MouseEvent -> handler) -> PropertyOrHandler handler
onDrop = on2 "onDrop" parseMouseEvent

onMouseDown :: (Event -> MouseEvent -> handler) -> PropertyOrHandler handler
onMouseDown = on2 "onMouseDown" parseMouseEvent

onMouseEnter :: (Event -> MouseEvent -> handler) -> PropertyOrHandler handler
onMouseEnter = on2 "onMouseEnter" parseMouseEvent

onMouseLeave :: (Event -> MouseEvent -> handler) -> PropertyOrHandler handler
onMouseLeave = on2 "onMouseLeave" parseMouseEvent

onMouseMove :: (Event -> MouseEvent -> handler) -> PropertyOrHandler handler
onMouseMove = on2 "onMouseMove" parseMouseEvent

onMouseOut :: (Event -> MouseEvent -> handler) -> PropertyOrHandler handler
onMouseOut = on2 "onMouseOut" parseMouseEvent

onMouseOver :: (Event -> MouseEvent -> handler) -> PropertyOrHandler handler
onMouseOver = on2 "onMouseOver" parseMouseEvent

onMouseUp :: (Event -> MouseEvent -> handler) -> PropertyOrHandler handler
onMouseUp = on2 "onMouseUp" parseMouseEvent

--------------------------------------------------------------------------------
-- Touch
--------------------------------------------------------------------------------

-- | Initialize touch events is only needed with React 0.13, in version 0.14 it was removed.
#ifdef __GHCJS__
foreign import javascript unsafe
    "React['initializeTouchEvents'] ? React['initializeTouchEvents'](true) : null"
    initializeTouchEvents :: IO ()
#else
initializeTouchEvents :: IO ()
initializeTouchEvents = return ()
#endif

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

parseTouch :: JSRef -> Touch
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

parseTouchList :: JSRef -> JSString -> [Touch]
parseTouchList obj key = unsafePerformIO $ do
    let arr = js_getArrayProp obj key
        len = arrayLength arr
    forM [0..len-1] $ \idx -> do
        let jsref = arrayIndex idx arr
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
onTouchCancel = on2 "onTouchCancel" parseTouchEvent

onTouchEnd :: (Event -> TouchEvent -> handler) -> PropertyOrHandler handler
onTouchEnd = on2 "onTouchEnd" parseTouchEvent

onTouchMove :: (Event -> TouchEvent -> handler) -> PropertyOrHandler handler
onTouchMove = on2 "onTouchMove" parseTouchEvent

onTouchStart :: (Event -> TouchEvent -> handler) -> PropertyOrHandler handler
onTouchStart = on2 "onTouchStart" parseTouchEvent

--------------------------------------------------------------------------------
-- UI Events
--------------------------------------------------------------------------------

onScroll :: (Event -> handler) -> PropertyOrHandler handler
onScroll = on "onScroll"

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

onWheel :: (Event -> MouseEvent -> WheelEvent -> handler) -> PropertyOrHandler handler
onWheel f = CallbackPropertyWithSingleArgument
    { csPropertyName = "onWheel"
    , csFunc = \raw -> f (parseEvent raw) (parseMouseEvent raw) (parseWheelEvent raw)
    }

--------------------------------------------------------------------------------
--- Image
--------------------------------------------------------------------------------

onLoad :: (Event -> handler) -> PropertyOrHandler handler
onLoad = on "onLoad"

onError :: (Event -> handler) -> PropertyOrHandler handler
onError = on "onError"

--------------------------------------------------------------------------------
--- JS Utils
--------------------------------------------------------------------------------

#ifdef __GHCJS__

foreign import javascript unsafe
    "$1[$2]"
    js_getProp :: JSRef -> JSString -> JSRef

foreign import javascript unsafe
    "$1[$2]"
    js_getArrayProp :: JSRef -> JSString -> JSArray

-- | Access a property from an object.  Since event objects are immutable, we can use
-- unsafePerformIO without worry.
(.:) :: FromJSRef b => JSRef -> JSString -> b
obj .: key = fromMaybe (error "Unable to decode event target") $ unsafePerformIO $
    fromJSRef $ js_getProp obj key

foreign import javascript unsafe
    "$1['getModifierState']($2)"
    js_GetModifierState :: JSRef -> JSString -> JSRef

getModifierState :: JSRef -> String -> Bool
getModifierState ref = fromJSBool . js_GetModifierState ref . toJSString

arrayLength :: JSArray -> Int
arrayLength = JSA.length

arrayIndex :: Int -> JSArray -> JSRef
arrayIndex = JSA.index

#else

js_getProp :: a -> String -> JSRef
js_getProp _ _ = ()

js_getArrayProp :: a -> String -> JSRef
js_getArrayProp _ _ = ()

(.:) :: JSRef -> String -> b
_ .: _ = undefined

getModifierState :: JSRef -> String -> Bool
getModifierState _ _ = False

arrayLength :: JSArray -> Int
arrayLength _ = 0

arrayIndex :: Int -> JSArray -> JSRef
arrayIndex _ _ = ()

#endif
