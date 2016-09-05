-- | Internal module for React.Flux
--
-- Normally you should not need to use anything in this module.  This module is only needed if you have
-- complicated interaction with third-party javascript rendering code.

module React.Flux.Internal(
    ReactViewRef(..)
  , ReactElementRef(..)
  , ReactThis(..)
  , HandlerArg(..)
  , PropertyOrHandler(..)
  , property
  , (&=)
  , ReactElement(..)
  , ReactElementM(..)
  , transHandler
  , elemString
  , elemText
  , elemJSString
  , elemShow
  , el
  , childrenPassedToView
  , elementToM
  , mkReactElement
  , exportViewToJs
  , toJSString
  , JSString
) where

import           Data.String (IsString(..))
import           Data.Aeson
import           Data.Typeable (Typeable)
import           Control.Monad.Writer
import           Control.Monad.Identity (Identity(..))
import qualified Data.Text as T

#ifdef __GHCJS__
import           Unsafe.Coerce
import qualified Data.JSString as JSS
import qualified Data.JSString.Text as JSS
import           JavaScript.Array (JSArray)
import qualified JavaScript.Array as JSA
import           GHCJS.Foreign.Callback
import qualified JavaScript.Object as JSO
import           GHCJS.Types (JSVal, JSString, IsJSVal, jsval)
import           GHCJS.Marshal (ToJSVal(..))
import           GHCJS.Foreign (jsNull)
import           React.Flux.Export
#else
import Data.Text (Text)
type JSVal = ()
class ToJSVal a
instance ToJSVal Value
instance ToJSVal Text
instance ToJSVal ()
class IsJSVal a
type JSArray = JSVal
type JSString = String
instance IsJSVal JSVal
instance IsJSVal JSString
instance ToJSVal JSString
#endif

-- type JSObject a = JSO.Object a

-- | This type is for the return value of @React.createClass@
newtype ReactViewRef props = ReactViewRef { reactViewRef :: JSVal }
instance IsJSVal (ReactViewRef props)

-- | This type is for the return value of @React.createElement@
newtype ReactElementRef = ReactElementRef { reactElementRef :: JSVal }
instance IsJSVal ReactElementRef

-- | The first parameter of an event handler registered with React.
newtype HandlerArg = HandlerArg JSVal
instance IsJSVal HandlerArg

-- | The this value during the rendering function
newtype ReactThis state props = ReactThis {reactThisRef :: JSVal }
instance IsJSVal (ReactThis state props)

instance Show HandlerArg where
    show _ = "HandlerArg"

-- | Either a property or an event handler.
--
-- The combination of all properties and event handlers are used to create the javascript object
-- passed as the second argument to @React.createElement@.
data PropertyOrHandler handler =
   forall ref. ToJSVal ref => Property
      { propertyName :: JSString
      , propertyVal :: ref
      }
 | forall ref. ToJSVal ref => PropertyFromContext
      { propFromThisName :: JSString
      , propFromThisVal :: JSVal -> ref -- ^ will be passed this.context
      }
 | NestedProperty
      { nestedPropertyName :: JSString
      , nestedPropertyVals :: [PropertyOrHandler handler]
      }
 | ElementProperty
      { elementPropertyName :: JSString
      , elementValue :: ReactElementM handler ()
      }
 | CallbackPropertyWithArgumentArray
      { caPropertyName :: JSString
      , caFunc :: JSArray -> IO handler
      }
 | CallbackPropertyWithSingleArgument
      { csPropertyName :: JSString
      , csFunc :: HandlerArg -> handler
      }
 | forall props. Typeable props => CallbackPropertyReturningView
      { cretPropertyName :: JSString
      , cretArgToProp :: JSArray -> IO props
      , cretView :: ReactViewRef props
      }

instance Functor PropertyOrHandler where
    fmap _ (Property name val) = Property name val
    fmap _ (PropertyFromContext name f) = PropertyFromContext name f
    fmap f (NestedProperty name vals) = NestedProperty name (map (fmap f) vals)
    fmap f (ElementProperty name (ReactElementM mkElem)) =
        ElementProperty name $ ReactElementM $ mapWriter (\((),e) -> ((), fmap f e)) mkElem
    fmap f (CallbackPropertyWithArgumentArray name h) = CallbackPropertyWithArgumentArray name (fmap f . h)
    fmap f (CallbackPropertyWithSingleArgument name h) = CallbackPropertyWithSingleArgument name (f . h)
    fmap _ (CallbackPropertyReturningView name f v) = CallbackPropertyReturningView name f v

-- | Create a property from anything that can be converted to a JSVal
property :: ToJSVal val => JSString -> val -> PropertyOrHandler handler
property = Property

-- | Create a property for anything that can be converted to a javascript value using the @ToJSVal@
-- class from the @ghcjs-base@ package..  This is just an infix version of 'property'.
(&=) :: ToJSVal a => JSString -> a -> PropertyOrHandler handler
n &= a = Property n a

-- | A React element is a node or list of nodes in a virtual tree.  Elements are the output of the
-- rendering functions of classes.  React takes the output of the rendering function (which is a
-- tree of elements) and then reconciles it with the actual DOM elements in the browser.  The
-- 'ReactElement' is a monoid, so dispite its name can represent more than one element.  Multiple
-- elements are rendered into the browser DOM as siblings.
data ReactElement eventHandler
    = ForeignElement
        { fName :: Either JSString (ReactViewRef Object)
        , fProps :: [PropertyOrHandler eventHandler]
        , fChild :: ReactElement eventHandler
        }
    | forall props. Typeable props => ViewElement
        { ceClass :: ReactViewRef props
        , ceKey :: Maybe JSVal
        , ceProps :: props
        , ceChild :: ReactElement eventHandler
        }
    | RawJsElement
        { rawTransform :: JSVal -> [ReactElementRef] -> IO ReactElementRef
        -- ^ first arg is this from render method, second argument is the rendering of 'rawChild'
        , rawChild :: ReactElement eventHandler
        }
    | ChildrenPassedToView
    | Content JSString
    | Append (ReactElement eventHandler) (ReactElement eventHandler)
    | EmptyElement

instance Monoid (ReactElement eventHandler) where
    mempty = EmptyElement
    mappend x y = Append x y

instance Functor ReactElement where
    fmap f (ForeignElement n p c) = ForeignElement n (map (fmap f) p) (fmap f c)
    fmap f (ViewElement n k p c) = ViewElement n k p (fmap f c)
    fmap f (RawJsElement t c) = RawJsElement t (fmap f c)
    fmap _ ChildrenPassedToView = ChildrenPassedToView
    fmap f (Append a b) = Append (fmap f a) (fmap f b)
    fmap _ (Content s) = Content s
    fmap _ EmptyElement = EmptyElement

-- | A writer monad for 'ReactElement's which is used in the rendering function of all views.
--
-- @do@ notation or the 'Monoid' instance is used to sequence sibling elements.
-- Child elements are specified via function application; the combinator creating an element takes
-- the child element as a parameter. The @OverloadedStrings@ extension is used to create plain text.
--
-- >ul_ $ do li_ (b_ "Hello")
-- >         li_ "World"
-- >         li_ $
-- >             ul_ (li_ "Nested" <> li_ "List")
--
-- would build something like
--
-- ><ul>
-- >  <li><b>Hello</b><li>
-- >  <li>World</li>
-- >  <li><ul>
-- >    <li>Nested</li>
-- >    <li>List</li>
-- >  </ul></li>
-- ></ul>
--
-- The "React.Flux.DOM" module contains a large number of combinators for creating HTML elements.
newtype ReactElementM eventHandler a = ReactElementM { runReactElementM :: Writer (ReactElement eventHandler) a }
    deriving (Functor, Applicative, Monad, Foldable)

-- | Create a 'ReactElementM' containing a given 'ReactElement'.
elementToM :: a -> ReactElement eventHandler -> ReactElementM eventHandler a
elementToM a e = ReactElementM (WriterT (Identity (a, e)))

instance (a ~ ()) => Monoid (ReactElementM eventHandler a) where
    mempty = elementToM () EmptyElement
    mappend e1 e2 =
        let ((),e1') = runWriter $ runReactElementM e1
            ((),e2') = runWriter $ runReactElementM e2
         in elementToM () $ Append e1' e2'

instance (a ~ ()) => IsString (ReactElementM eventHandler a) where
    fromString s = elementToM () $ Content $ toJSString s

-- | Transform the event handler for a 'ReactElementM'.
transHandler :: (handler1 -> handler2) -> ReactElementM handler1 a -> ReactElementM handler2 a
transHandler f (ReactElementM writer) = ReactElementM $ mapWriter f' writer
  where
    f' (a, x) = (a, fmap f x)

-- | Create a text element from a string. The text content is escaped to be HTML safe.
-- If you need to insert HTML, instead use the
-- <https://facebook.github.io/react/tips/dangerously-set-inner-html.html dangerouslySetInnerHTML>
-- property.  This is an alias for 'fromString'.
elemString :: String -> ReactElementM eventHandler ()
elemString s = elementToM () $ Content $ toJSString s

-- | Create a text element from a text value. The text content is escaped to be HTML safe.
elemText :: T.Text -> ReactElementM eventHandler ()
#ifdef __GHCJS__
elemText s = elementToM () $ Content $ JSS.textToJSString s
#else
elemText s = elementToM () $ Content $ T.unpack s
#endif

-- | Create a text element from a @JSString@.  This is more efficient for hard-coded strings than
-- converting from text to a JavaScript string.  The string is escaped to be HTML safe.
elemJSString :: JSString -> ReactElementM eventHandler ()
elemJSString s = elementToM () $ Content s

-- | Create an element containing text which is the result of 'show'ing the argument.
-- Note that the resulting string is then escaped to be HTML safe.
elemShow :: Show a => a -> ReactElementM eventHandler ()
elemShow s = elementToM () $ Content $ toJSString $ show s

-- | Create a React element.
el :: JSString -- ^ The element name (the first argument to @React.createElement@).
   -> [PropertyOrHandler eventHandler] -- ^ The properties to pass to the element (the second argument to @React.createElement@).
   -> ReactElementM eventHandler a -- ^ The child elements (the third argument to @React.createElement@).
   -> ReactElementM eventHandler a
el name attrs (ReactElementM child) =
    let (a, childEl) = runWriter child
     in elementToM a $ ForeignElement (Left name) attrs childEl

-- | Transclude the children passed into 'React.Flux.view' or 'React.Flux.viewWithKey' into the
-- current rendering.  Use this where you would use @this.props.children@ in a javascript React
-- class.
childrenPassedToView :: ReactElementM eventHandler ()
childrenPassedToView = elementToM () ChildrenPassedToView

----------------------------------------------------------------------------------------------------
-- mkReactElement has two versions
----------------------------------------------------------------------------------------------------

type CallbackToRelease = JSVal

-- | Execute a ReactElementM to create a javascript React element and a list of callbacks attached
-- to nodes within the element.  These callbacks will need to be released with 'releaseCallback'
-- once the class is re-rendered.
mkReactElement :: forall eventHandler state props.
                  (eventHandler -> IO ())
               -> ReactThis state props -- ^ this
               -> ReactElementM eventHandler ()
               -> IO (ReactElementRef, [CallbackToRelease])

#ifdef __GHCJS__

mkReactElement runHandler this = runWriterT . mToElem
    where
        -- Run the ReactElementM monad to create a ReactElementRef.
        mToElem :: ReactElementM eventHandler () -> MkReactElementM ReactElementRef
        mToElem eM = do
            let e = execWriter $ runReactElementM eM
                e' = case e of
                        Content txt -> ForeignElement (Right $ ReactViewRef js_textWrapper) [] (Content txt)
                        _ -> e
            refs <- createElement e'
            case refs of
                [] -> lift $ js_ReactCreateElementNoChildren js_divLikeElement
                [x] -> return x
                xs -> lift $ do
                    emptyObj <- JSO.create
                    let arr = jsval $ JSA.fromList $ map reactElementRef xs
                    js_ReactCreateForeignElement (ReactViewRef js_divLikeElement) emptyObj arr

        -- add the property or handler to the javascript object
        addPropOrHandlerToObj :: JSO.Object -> PropertyOrHandler eventHandler -> MkReactElementM ()
        addPropOrHandlerToObj obj (Property n val) = lift $ do
            vRef <- toJSVal val
            JSO.setProp n vRef obj
        addPropOrHandlerToObj obj (PropertyFromContext n f) = lift $ do
            ctx <- js_ReactGetContext this
            vRef <- toJSVal $ f ctx
            JSO.setProp n vRef obj
        addPropOrHandlerToObj obj (NestedProperty n vals) = do
            nested <- lift $ JSO.create
            mapM_ (addPropOrHandlerToObj nested) vals
            lift $ JSO.setProp n (jsval nested) obj
        addPropOrHandlerToObj obj (ElementProperty name rM) = do
            ReactElementRef ref <- mToElem rM
            lift $ JSO.setProp name ref obj
        addPropOrHandlerToObj obj (CallbackPropertyWithArgumentArray name func) = do
            -- this will be released by the render function of the class (jsbits/class.js)
            cb <- lift $ syncCallback1 ContinueAsync $ \argref -> do
                handler <- func $ unsafeCoerce argref
                runHandler handler
            tell [jsval cb]
            wrappedCb <- lift $ js_CreateArgumentsCallback cb
            lift $ JSO.setProp name wrappedCb obj
        addPropOrHandlerToObj obj (CallbackPropertyWithSingleArgument name func) = do
            -- this will be released by the render function of the class (jsbits/class.js)
            cb <- lift $ syncCallback1 ContinueAsync $ \ref ->
                runHandler $ func $ HandlerArg ref
            tell [jsval cb]
            lift $ JSO.setProp name (jsval cb) obj

        addPropOrHandlerToObj obj (CallbackPropertyReturningView name toProps v) = do
            (cb, wrappedCb) <- lift $ exportViewToJs v toProps
            tell [cb]
            lift $ JSO.setProp name wrappedCb obj

        -- call React.createElement
        createElement :: ReactElement eventHandler -> MkReactElementM [ReactElementRef]
        createElement EmptyElement = return []
        createElement (Append x y) = (++) <$> createElement x <*> createElement y
        createElement (Content s) = return [js_ReactCreateContent s]
        createElement ChildrenPassedToView = lift $ do
          childRef <- js_ReactGetChildren this
          return $ map ReactElementRef $ JSA.toList childRef
        createElement (f@(ForeignElement{})) = do
            obj <- lift $ JSO.create
            mapM_ (addPropOrHandlerToObj obj) $ fProps f
            childNodes <- createElement $ fChild f
            let children = case map reactElementRef childNodes of
                             [] -> jsNull
                             [x] -> x
                             xs -> jsval $ JSA.fromList xs
            e <- lift $ case fName f of
                Left s -> js_ReactCreateElementName s obj children
                Right ref -> js_ReactCreateForeignElement ref obj children
            return [e]
        createElement (ViewElement { ceClass = rc, ceProps = props, ceKey = mkey, ceChild = child }) = do
            childNodes <- createElement child
            propsE <- lift $ export props -- this will be released inside the lifetime events for the class (jsbits/class.js)

            let children = case map reactElementRef childNodes of
                             [] -> jsNull
                             [x] -> x
                             xs -> jsval $ JSA.fromList xs

            e <- lift $ case mkey of
                Just keyRef -> js_ReactCreateKeyedElement rc keyRef propsE children
                Nothing -> js_ReactCreateClass rc propsE children
            return [e]

        createElement (RawJsElement trans child) = do
            childNodes <- createElement child
            e <- liftIO $ trans (reactThisRef this) childNodes
            return [e]

type MkReactElementM a = WriterT [CallbackToRelease] IO a

foreign import javascript unsafe
    "React['createElement']($1)"
    js_ReactCreateElementNoChildren :: JSVal -> IO ReactElementRef

foreign import javascript unsafe
    "React['createElement']($1, $2, $3)"
    js_ReactCreateElementName :: JSString -> JSO.Object -> JSVal -> IO ReactElementRef

foreign import javascript unsafe
    "React['createElement']($1, $2, $3)"
    js_ReactCreateForeignElement :: ReactViewRef a -> JSO.Object -> JSVal -> IO ReactElementRef

foreign import javascript unsafe
    "React['createElement']($1, {hs:$2}, $3)"
    js_ReactCreateClass :: ReactViewRef a -> Export props -> JSVal -> IO ReactElementRef

foreign import javascript unsafe
    "React['createElement']($1, {key: $2, hs:$3}, $4)"
    js_ReactCreateKeyedElement :: ReactViewRef a -> JSVal -> Export props -> JSVal -> IO ReactElementRef

foreign import javascript unsafe
    "hsreact$mk_arguments_callback($1)"
    js_CreateArgumentsCallback :: Callback (JSVal -> IO ()) -> IO JSVal

foreign import javascript unsafe
    "hsreact$wrap_callback_returning_element($1)"
    js_wrapCallbackReturningElement :: Callback (JSVal -> JSVal -> IO ()) -> IO JSVal

foreign import javascript unsafe
    "$1.elem = $2"
    js_setElemReturnFromCallback :: JSVal -> ReactElementRef -> IO ()

foreign import javascript unsafe
    "$r = hsreact$divLikeElement"
    js_divLikeElement :: JSVal

foreign import javascript unsafe
    "$r = hsreact$textWrapper"
    js_textWrapper :: JSVal

foreign import javascript unsafe
    "$1['context']"
    js_ReactGetContext :: ReactThis state props -> IO JSVal

foreign import javascript unsafe
    "hsreact$children_to_array($1['props']['children'])"
    js_ReactGetChildren :: ReactThis state props -> IO JSArray


js_ReactCreateContent :: JSString -> ReactElementRef
js_ReactCreateContent = ReactElementRef . unsafeCoerce

toJSString :: String -> JSString
toJSString = JSS.pack


exportViewToJs :: Typeable props => ReactViewRef props -> (JSArray -> IO props) -> IO (CallbackToRelease, JSVal)
exportViewToJs view toProps = do
    cb <- syncCallback2 ContinueAsync $ \ret argref -> do
        props <- toProps $ unsafeCoerce argref
        propsE <- export props -- this will be released inside the lifetime events for the class
        e <- js_ReactCreateClass view propsE jsNull
        js_setElemReturnFromCallback ret e
    wrappedCb <- js_wrapCallbackReturningElement cb
    return (jsval cb, wrappedCb)

#else
mkReactElement _ _ _ = return (ReactElementRef (), [])

toJSString :: String -> String
toJSString = id

exportViewToJs :: Typeable props => ReactViewRef props -> (JSArray -> IO props) -> IO (CallbackToRelease, JSVal)
exportViewToJs _ _ = return ((), ())

#endif
