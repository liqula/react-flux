-- | This module contains the definitions for creating properties to pass to javascript elements and
-- foreign javascript classes.  In addition, it contains definitions for the
-- <https://facebook.github.io/react/docs/events.html React Event System>.
{-# LANGUAGE ViewPatterns, UndecidableInstances #-}
module React.Flux.PropertiesAndEvents (
    PropertyOrHandler

  -- * Creating Properties
  , property
  , elementProperty
  , nestedProperty
  , CallbackFunction
  , callback
  , foreignClass
  , rawJsRendering

  -- ** Combinators
  , (@=)
  , ($=)
  , (&=)
  , classNames
  , classNamesLast
  , classNamesAny

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
import           Control.Monad.Writer (runWriter)
import           Control.DeepSeq
import           System.IO.Unsafe (unsafePerformIO)
import           Data.Monoid ((<>))
import qualified Data.Text as T
import           GHC.Generics

import           React.Flux.Internal
import           React.Flux.Views (ViewEventHandler, StatefulViewEventHandler)

import           Data.Maybe (fromMaybe)

import           GHCJS.Foreign (fromJSBool)
import           GHCJS.Marshal (FromJSVal(..))
import           GHCJS.Types (JSVal, nullRef, IsJSVal)
import           JavaScript.Array as JSA
import qualified Data.JSString.Text as JSS


-- | Some third-party React classes allow passing React elements as properties.  This function
-- will first run the given 'ReactElementM' to obtain an element or elements, and then use that
-- element as the value for a property with the given key.
elementProperty :: JSString -> ReactElementM handler () -> PropertyOrHandler handler
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
nestedProperty :: JSString -> [PropertyOrHandler handler] -> PropertyOrHandler handler
nestedProperty = NestedProperty

-- | A class which is used to implement <https://wiki.haskell.org/Varargs variable argument functions>.
-- Any function where each argument implements 'FromJSVal' and the result is either
-- 'ViewEventHandler' or 'StatefulViewEventHandler' is an instance of this class.
class CallbackFunction handler a | a -> handler  where
    applyFromArguments :: JSArray -> Int -> a -> IO handler

instance CallbackFunction ViewEventHandler ViewEventHandler where
    applyFromArguments _ _ h = return h

instance {-# OVERLAPPING #-} CallbackFunction (StatefulViewEventHandler s) (StatefulViewEventHandler s) where
    applyFromArguments _ _ h = return h

instance {-# OVERLAPPABLE #-} (FromJSVal a, CallbackFunction handler b) => CallbackFunction handler (a -> b) where
    applyFromArguments args k f = do
        ma <- fromJSVal $ if k >= JSA.length args then nullRef else JSA.index k args
        a <- maybe (error "Unable to decode callback argument") return ma
        applyFromArguments args (k+1) $ f a

-- | Create a callback property.  This is primarily intended for foreign React classes which expect
-- callbacks to be passed to them as properties.  For events on DOM elements, you should instead use
-- the handlers below.
--
-- The function @func@ can be any function, as long as each argument to the function is an instance
-- of 'FromJSVal' and the result of the function is @handler@.  Internally, 'callback' creates a
-- javascript function which accesses the @arguments@ javascript object and then matches entries in
-- @arguments@ to the parameters of @func@.  If @func@ has more parameters than the javascript
-- @arguments@ object, a javascript null is used for the conversion.  Since the 'Maybe' instance of
-- 'FromJSVal' converts a null reference to 'Nothing', you can exploit this to create
-- variable-argument javascript callbacks.
--
-- For example, all three of the following functions could be passed as @func@ inside a view.
--
-- >foo :: Int -> Maybe String -> ViewEventHandler
-- >bar :: Aeson.Value -> ViewEventHandler
-- >baz :: ViewEventHandler
--
-- For another example, see the haddock comments in "React.Flux.Addons.Bootstrap".
callback :: CallbackFunction handler func => JSString -> func -> PropertyOrHandler handler
callback name func = CallbackPropertyWithArgumentArray name $ \arr -> applyFromArguments arr 0 func


-- | Create a 'ReactElement' for a class defined in javascript.  See
-- 'React.Flux.Combinators.foreign_' for a convenient wrapper and some examples.
foreignClass :: JSVal -- ^ The javascript reference to the class
             -> [PropertyOrHandler eventHandler] -- ^ properties and handlers to pass when creating an instance of this class.
             -> ReactElementM eventHandler a -- ^ The child element or elements
             -> ReactElementM eventHandler a
foreignClass name attrs (ReactElementM child) =
    let (a, childEl) = runWriter child
     in elementToM a $ ForeignElement (Right $ ReactViewRef name) attrs childEl


-- | Inject arbitrary javascript code into the rendering function.  This is very low level and should only
-- be used as a last resort when interacting with complex third-party react classes.  For the most part,
-- third-party react classes can be interacted with using 'foreignClass' and the various ways of creating
-- properties.
rawJsRendering :: (JSVal -> JSArray -> IO JSVal)
                  -- ^ The raw code to inject into the rendering function.  The first argument is the 'this' value
                  -- from the rendering function so points to the react class.  The second argument is the result of
                  -- rendering the children so is an array of react elements.  The return value must be a React element.
               -> ReactElementM handler () -- ^ the children
               -> ReactElementM handler ()
rawJsRendering trans (ReactElementM child) =
    let (a, childEl) = runWriter child
        trans' thisVal childLst =
          ReactElementRef <$> trans thisVal (JSA.fromList $ map reactElementRef childLst)
     in elementToM a $ RawJsElement trans' childEl

----------------------------------------------------------------------------------------------------
--- Generic Event
----------------------------------------------------------------------------------------------------

-- | A reference to the object that dispatched the event.
-- <https://developer.mozilla.org/en-US/docs/Web/API/Event/target>
newtype EventTarget = EventTarget JSVal
  deriving (Generic)
instance IsJSVal EventTarget
instance NFData EventTarget

instance Show (EventTarget) where
    show _ = "EventTarget"

-- | Access a property in an event target
eventTargetProp :: FromJSVal val => EventTarget -> JSString -> val
eventTargetProp (EventTarget ref) key = ref .: key

-- | Every event in React is a synthetic event, a cross-browser wrapper around the native event.
data Event = Event
    { evtType :: T.Text
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
    } deriving (Show, Generic)

instance NFData Event

-- | A version of 'eventTargetProp' which accesses the property of 'evtTarget' in the event.  This
-- is useful for example:
--
-- >div_ $
-- >    input_ [ "type" @= "checked"
-- >           , onChange $ \evt -> let val = target evt "value" in ...
-- >           ]
--
-- In this case, @val@ would coorespond to the javascript expression @evt.target.value@.
target :: FromJSVal val => Event -> JSString -> val
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

-- | Use this to create an event handler for an event not covered by the rest of this module.
-- (Events are not covered if they don't have extra arguments that require special handling.)
-- For example, onPlay and onPause are events you could use with @on@.
on :: JSString -> (Event -> handler) -> PropertyOrHandler handler
on name f = CallbackPropertyWithSingleArgument
    { csPropertyName = name
    , csFunc = f . parseEvent
    }

-- | Construct a handler from a detail parser, used by the various events below.
on2 :: JSString -- ^ The event name
    -> (HandlerArg -> detail) -- ^ A function parsing the details for the specific event.
    -> (Event -> detail -> handler) -- ^ The function implementing the handler.
    -> PropertyOrHandler handler
on2 name parseDetail f = CallbackPropertyWithSingleArgument
    { csPropertyName = name
    , csFunc = \raw -> f (parseEvent raw) (parseDetail raw)
    }

-- | React re-uses event objects in a pool.  To make sure this is OK, we must perform
-- all computation involving the event object before it is returned to React.  But the callback
-- registered in the handler will return anytime the Haskell thread blocks, and the Haskell thread
-- will continue asynchronously.  If this occurs, the event object is no longer valid.  This
-- therefore needs to be called and fully processed in the handler before anything else happens,
-- like sending actions to stores.
--
-- TODO: this requires some more reasoning that would show that it actually works.  It may be
-- possible that there still is room for the thread to yield between receiving the handler object
-- and executing the 'unsafePerformIO' in here, even if it is called first thing in the handler
-- body.
--
-- FIXME: A better way may be to make a specialized monad available in the event handlers that
-- allows for these things, but not general IO.
preventDefault :: Event -> ()
preventDefault (evtHandlerArg -> HandlerArg ref) = unsafePerformIO (js_preventDefault ref) `seq` ()

-- | See 'preventDefault'.
stopPropagation :: Event -> ()
stopPropagation (evtHandlerArg -> HandlerArg ref) = unsafePerformIO (js_stopPropagation ref) `seq` ()

foreign import javascript unsafe
    "$1['preventDefault']();"
    js_preventDefault :: JSVal -> IO ()

foreign import javascript unsafe
    "$1['stopPropagation']();"
    js_stopPropagation :: JSVal -> IO ()

-- | By default, the handlers below are triggered during the bubbling phase.  Use this to switch
-- them to trigger during the capture phase.
capturePhase :: PropertyOrHandler handler -> PropertyOrHandler handler
capturePhase (CallbackPropertyWithSingleArgument n h) = CallbackPropertyWithSingleArgument (n <> "Capture") h
capturePhase _ = error "You must use React.Flux.PropertiesAndEvents.capturePhase on an event handler"


---------------------------------------------------------------------------------------------------
--- Keyboard
---------------------------------------------------------------------------------------------------

-- | The data for the keyboard events
data KeyboardEvent = KeyboardEvent
  { keyEvtAltKey :: Bool
  , keyEvtCharCode :: Int
  , keyEvtCtrlKey :: Bool
  , keyGetModifierState :: T.Text -> Bool
  , keyKey :: T.Text
  , keyCode :: Int
  , keyLocale :: Maybe T.Text
  , keyLocation :: Int
  , keyMetaKey :: Bool
  , keyRepeat :: Bool
  , keyShiftKey :: Bool
  , keyWhich :: Int
  }
  deriving (Generic)

instance NFData KeyboardEvent
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
  , mouseGetModifierState :: T.Text -> Bool
  , mouseMetaKey :: Bool
  , mousePageX :: Int
  , mousePageY :: Int
  , mouseRelatedTarget :: EventTarget
  , mouseScreenX :: Int
  , mouseScreenY :: Int
  , mouseShiftKey :: Bool
  }
  deriving (Generic)

instance NFData MouseEvent
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
foreign import javascript unsafe
    "React['initializeTouchEvents'] ? React['initializeTouchEvents'](true) : null"
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
} deriving (Show, Generic)

instance NFData Touch

data TouchEvent = TouchEvent {
    touchAltKey :: Bool
  , changedTouches :: [Touch]
  , touchCtrlKey :: Bool
  , touchGetModifierState :: T.Text -> Bool
  , touchMetaKey :: Bool
  , touchShiftKey :: Bool
  , touchTargets :: [Touch]
  , touches :: [Touch]
  }
  deriving (Generic)

instance NFData TouchEvent

instance Show TouchEvent where
    show (TouchEvent t1 t2 t3 _ t4 t5 t6 t7)
        = show (t1, t2, t3, t4, t5, t6, t7)

parseTouch :: JSVal -> Touch
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

parseTouchList :: JSVal -> JSString -> [Touch]
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
} deriving (Show, Generic)

instance NFData WheelEvent

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

foreign import javascript unsafe
    "$1[$2]"
    js_getProp :: JSVal -> JSString -> JSVal

foreign import javascript unsafe
    "$1[$2]"
    js_getArrayProp :: JSVal -> JSString -> JSArray

-- | Access a property from an object.  Since event objects are immutable, we can use
-- unsafePerformIO without worry.
(.:) :: FromJSVal b => JSVal -> JSString -> b
obj .: key = fromMaybe (error "Unable to decode event target") $ unsafePerformIO $  -- TODO: get rid of the unsafePerformIO here!
    fromJSVal $ js_getProp obj key

foreign import javascript unsafe
    "$1['getModifierState']($2)"
    js_GetModifierState :: JSVal -> JSString -> JSVal

getModifierState :: JSVal -> T.Text -> Bool
getModifierState ref = fromJSBool . js_GetModifierState ref . JSS.textToJSString

arrayLength :: JSArray -> Int
arrayLength = JSA.length

arrayIndex :: Int -> JSArray -> JSVal
arrayIndex = JSA.index
