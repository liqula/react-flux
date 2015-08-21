-- | Internal module for React.Flux
--
-- Normally you should not need to use anything in this module.  This module is only needed if you have
-- complicated interaction with third-party javascript rendering code.
module React.Flux.Internal(
    ReactViewRef(..)
  , ReactViewKey(..)
  , ReactElementRef(..)
  , HandlerArg(..)
  , PropertyOrHandler(..)
  , ReactElement(..)
  , ReactElementM(..)
  , elemText
  , elemShow
  , el
  , childrenPassedToView
  , elementToM
  , mkReactElement
  , toJSString
) where

import           Data.String (IsString(..))
import           Data.Aeson
import           Data.Aeson.Types (Pair)
import           Data.Typeable (Typeable)
import           Control.Monad.Writer
import           Control.Monad.Identity (Identity(..))
import qualified Data.Text as T

#ifdef __GHCJS__
import           Unsafe.Coerce
import qualified Data.JSString as JSS
import qualified JavaScript.Array as JSA
import           GHCJS.Foreign.Callback
import qualified JavaScript.Object as JSO
import           GHCJS.Types (JSRef, castRef, JSString)
import           GHCJS.Marshal (toJSRef_aeson, ToJSRef(..), fromJSRef)
import           React.Flux.Export
#else
type JSRef a = ()
type JSFun a = JSRef a
#endif

-- type JSObject a = JSO.Object a

-- | This type is for the return value of @React.createClass@
newtype ReactViewRef props = ReactViewRef { reactViewRef :: JSRef () }

-- | This type is for the return value of @React.createElement@
newtype ReactElementRef = ReactElementRef { reactElementRef :: JSRef () }

-- | The first parameter of an event handler registered with React.
newtype HandlerArg = HandlerArg (JSRef ())

instance Show HandlerArg where
    show _ = "HandlerArg"

-- | Either a property or an event handler.
--
-- The combination of all properties and event handlers are used to create the javascript object
-- passed as the second argument to @React.createElement@.
data PropertyOrHandler handler =
   Property Pair
 | EventHandler
      { evtHandlerName :: String
      , evtHandler :: HandlerArg -> handler
      }
 | CallbackProperty
      { callbackName :: String
      , callbackFn :: Value -> handler
      }

instance Functor PropertyOrHandler where
    fmap _ (Property p) = Property p
    fmap f (EventHandler name h) = EventHandler name (f . h)
    fmap f (CallbackProperty name g) = CallbackProperty name (f . g)

-- | Keys in React can either be strings or integers
class ReactViewKey key where
    toKeyRef :: key -> IO (JSRef ())

#if __GHCJS__
instance ReactViewKey String where
    toKeyRef = return . unsafeCoerce . toJSString

instance ReactViewKey Int where
    toKeyRef i = castRef <$> toJSRef i
#else
instance ReactViewKey String where
    toKeyRef = const $ return ()

instance ReactViewKey Int where
    toKeyRef = const $ return ()
#endif

-- | A React element is a node or list of nodes in a virtual tree.  Elements are the output of the
-- rendering functions of classes.  React takes the output of the rendering function (which is a
-- tree of elements) and then reconciles it with the actual DOM elements in the browser.  The
-- 'ReactElement' is a monoid, so dispite its name can represent more than one element.  Multiple
-- elements are rendered into the browser DOM as siblings.
data ReactElement eventHandler
    = ForeignElement
        { fName :: Either String (ReactViewRef Object)
        , fProps :: [PropertyOrHandler eventHandler]
        , fChild :: ReactElement eventHandler
        }
    | forall props key. (Typeable props, ReactViewKey key) => ViewElement
        { ceClass :: ReactViewRef props
        , ceKey :: Maybe key
        -- TODO: ref?  ref support would need to tie into the Class too.
        , ceProps :: props
        , ceChild :: ReactElement eventHandler
        }
    | ChildrenPassedToView
    | Content String
    | Append (ReactElement eventHandler) (ReactElement eventHandler)
    | EmptyElement

instance Monoid (ReactElement eventHandler) where
    mempty = EmptyElement
    mappend x y = Append x y

instance Functor ReactElement where
    fmap f (ForeignElement n p c) = ForeignElement n (map (fmap f) p) (fmap f c)
    fmap f (ViewElement n k p c) = ViewElement n k p (fmap f c)
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
    fromString s = elementToM () $ Content s

-- | Create a text element from a string.  This is an alias for 'fromString'.  The text content is
-- escaped to be HTML safe.  If you need to insert HTML, instead use the
-- <https://facebook.github.io/react/tips/dangerously-set-inner-html.html dangerouslySetInnerHTML>
-- property.
elemText :: String -> ReactElementM eventHandler ()
elemText s = elementToM () $ Content s

-- | Create an element containing text which is the result of 'show'ing the argument.
-- Note that the resulting string is then escaped to be HTML safe.
elemShow :: Show a => a -> ReactElementM eventHandler ()
elemShow s = elementToM () $ Content $ show s

-- | Create a React element.
el :: String -- ^ The element name (the first argument to @React.createElement@).
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

-- | Execute a ReactElementM to create a javascript React element and a list of callbacks attached
-- to nodes within the element.  These callbacks will need to be released with 'releaseCallback'
-- once the class is re-rendered.
mkReactElement :: (eventHandler -> IO ())
               -> IO [ReactElementRef] -- ^ this.props.children
               -> ReactElementM eventHandler ()
               -> IO (ReactElementRef, [Callback (JSRef () -> IO ())])

#ifdef __GHCJS__

mkReactElement runHandler getPropsChildren eM = runWriterT $ do
    let e = execWriter $ runReactElementM eM
    refs <- createElement getPropsChildren $ fmap runHandler e
    case refs of
        [] -> lift $ js_ReactCreateElementNoChildren "div"
        [x] -> return x
        xs -> lift $ do
            emptyObj <- JSO.create
            let arr = JSA.fromList $ map reactElementRef xs
            js_ReactCreateElementName "div" emptyObj arr

foreign import javascript unsafe
    "React['createElement']($1)"
    js_ReactCreateElementNoChildren :: JSString -> IO ReactElementRef

foreign import javascript unsafe
    "React['createElement']($1, $2, $3)"
    js_ReactCreateElementName :: JSString -> JSO.Object -> JSA.JSArray -> IO ReactElementRef

foreign import javascript unsafe
    "React['createElement']($1, $2, $3)"
    js_ReactCreateForeignElement :: ReactViewRef a -> JSO.Object -> JSA.JSArray -> IO ReactElementRef

foreign import javascript unsafe
    "React['createElement']($1, {hs:$2}, $3)"
    js_ReactCreateClass :: ReactViewRef a -> Export props -> JSA.JSArray -> IO ReactElementRef

foreign import javascript unsafe
    "React['createElement']($1, {key: $2, hs:$3}, $4)"
    js_ReactCreateKeyedElement :: ReactViewRef a -> JSRef key -> Export props -> JSA.JSArray -> IO ReactElementRef

js_ReactCreateContent :: String -> ReactElementRef
js_ReactCreateContent = ReactElementRef . unsafeCoerce . toJSString

addPropOrHandlerToObj :: JSO.Object -> PropertyOrHandler (IO ()) -> WriterT [Callback (JSRef () -> IO ())] IO ()
addPropOrHandlerToObj obj (Property (n, v)) = lift $ do
    vRef <- toJSRef_aeson v
    JSO.setProp (toJSString $ T.unpack n) vRef obj
addPropOrHandlerToObj obj (EventHandler str handler) = do
    -- this will be released by the render function of the class (jsbits/class.js)
    cb <- lift $ syncCallback1 ContinueAsync $ \evtRef ->
        handler $ HandlerArg evtRef
    tell [cb]
    cbRef <- lift $ toJSRef cb
    lift $ JSO.setProp (toJSString str) cbRef obj
addPropOrHandlerToObj obj (CallbackProperty str handler) = do
    cb <- lift $ syncCallback1 ContinueAsync $ \argref -> do
        v <- fromJSRef $ castRef argref
        handler $ maybe (error "Unable to decode callback value") id v
    tell [cb]
    cbRef <- lift $ toJSRef cb
    lift $ JSO.setProp (toJSString str) cbRef obj

createElement :: IO [ReactElementRef] -> ReactElement (IO ()) -> WriterT [Callback (JSRef () -> IO ())] IO [ReactElementRef]
createElement _ EmptyElement = return []
createElement c (Append x y) = (++) <$> createElement c x <*> createElement c y
createElement _ (Content s) = return [js_ReactCreateContent s]
createElement c ChildrenPassedToView = lift c
createElement c (f@(ForeignElement{})) = do
    obj <- lift $ JSO.create
    mapM_ (addPropOrHandlerToObj obj) $ fProps f
    childNodes <- createElement c $ fChild f
    let childArr = JSA.fromList $ map reactElementRef childNodes
    e <- lift $ case fName f of
        Left s -> js_ReactCreateElementName (toJSString s) obj childArr
        Right ref -> js_ReactCreateForeignElement ref obj childArr
    return [e]
createElement c (ViewElement { ceClass = rc, ceProps = props, ceKey = mkey, ceChild = child }) = do
    childNodes <- createElement c child
    propsE <- lift $ export props -- this will be released inside the lifetime events for the class (jsbits/class.js)
    let arr = JSA.fromList $ map reactElementRef childNodes
    e <- lift $ case mkey of
        Just key -> do
            keyRef <- toKeyRef key
            js_ReactCreateKeyedElement rc keyRef propsE arr
        Nothing -> js_ReactCreateClass rc propsE arr
    return [e]

toJSString :: String -> JSString
toJSString = JSS.pack

#else
toJSString :: String -> String
toJSString = id

mkReactElement _ _ _ = return (ReactElementRef (), [])

#endif
