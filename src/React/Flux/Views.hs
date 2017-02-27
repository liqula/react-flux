-- | Internal module containing the view definitions
{-# LANGUAGE UndecidableInstances, AllowAmbiguousTypes, TypeApplications, MagicHash #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
module React.Flux.Views where

import Control.Monad.Writer
import Data.Typeable
import Control.DeepSeq
import qualified Data.HashMap.Strict as M

import React.Flux.Store
import React.Flux.Internal

#ifdef __GHCJS__
import System.IO.Unsafe (unsafePerformIO)
import React.Flux.Export
import JavaScript.Array
import GHCJS.Foreign.Callback
import GHCJS.Types (JSVal, IsJSVal, nullRef, jsval)
import GHCJS.Marshal (ToJSVal(..), FromJSVal(..))
import GHCJS.Marshal.Pure (PToJSVal(..))
import qualified JavaScript.Array as JSA

#else
type JSVal = ()
type JSArray = ()
type Callback x = ()
class FromJSVal a
pToJSVal :: a -> JSVal
pToJSVal _ = ()
#endif


-- | A view is conceptually a rendering function from @props@ and some internal state to a tree of elements.  The function
-- receives a value of type @props@ from its parent in the virtual DOM.  Additionally, the rendering
-- function can depend on some internal state or store data.  Based on the @props@ and the internal
-- state, the rendering function produces a virtual tree of elements which React then reconciles
-- with the browser DOM.
--
-- This module supports 3 kinds of views.  All of the views provided by this module are pure, in the
-- sense that the rendering function and event handlers cannot perform any IO.  All IO occurs inside
-- the 'transform' function of a store.
--
-- Due to React limitations (see <https://github.com/facebook/react/issues/2127 issue2127>), React
-- views must have a single top-level element.  If your haskell code returns multiple top-level
-- elements, react-flux will wrap them in a container @div@.  You should not rely on this and instead
-- make sure each view returns only a single top-level element (such as @todoItem@ below returning only
-- a single @li@ element).
newtype ReactView props = ReactView { reactView :: ReactViewRef props }

---------------------------------------------------------------------------------------------------
--- Two versions of defineControllerView
---------------------------------------------------------------------------------------------------

-- | Event handlers in a controller-view and a view transform events into actions, but are not
-- allowed to perform any 'IO'.
type ViewEventHandler = [SomeStoreAction]

-- | A controller view provides the glue between a 'ReactStore' and the DOM.
-- The controller-view registers with the given store, and whenever the store is transformed the
-- controller-view re-renders itself.  Each instance of a controller-view also accepts properties of
-- type @props@ from its parent.  Whenever the parent re-renders itself, the new properties will be
-- passed down to the controller-view causing it to re-render itself.
--
-- Events registered on controller-views are expected to produce lists of 'SomeStoreAction'.  Since
-- lists of 'SomeStoreAction' are the output of the dispatcher, each event handler should just be a
-- call to a dispatcher function.  Once the event fires, the actions are executed causing the
-- store(s) to transform which leads to the controller-view(s) re-rendering.  This one-way flow of
-- data from actions to store to controller-views is central to the flux design.
--
-- It is recommended to have one controller-view for each
-- significant section of the page.  Controller-views deeper in the page tree can cause complexity
-- because data is now flowing into the page in multiple possibly conflicting places.  You must
-- balance the gain of encapsulated components versus the complexity of multiple entry points for
-- data into the page.  Note that multiple controller views can register with the same store.
--
-- >todoApp :: ReactView ()
-- >todoApp = defineControllerView "todo app" todoStore $ \todoState () ->
-- >    div_ $ do
-- >        todoHeader_
-- >        mainSection_ todoState
-- >        todoFooter_ todoState
defineControllerView :: (StoreData storeData, Typeable props)
                 => JSString -- ^ A name for this view, used only for debugging/console logging
                 -> ReactStore storeData -- ^ The store this controller view should attach to.
                 -> (storeData -> props -> ReactElementM ViewEventHandler ()) -- ^ The rendering function
                 -> ReactView props

#ifdef __GHCJS__

defineControllerView name (ReactStore store _) buildNode = unsafePerformIO $ do
    let render sd props = return $ buildNode sd props
    renderCb <- mkRenderCallback (js_ReactGetState >=> parseExport) runViewHandler render
    ReactView <$> js_createControllerView name store renderCb

-- | Transform a controller view handler to a raw handler.
runViewHandler :: ReactThis state props -> ViewEventHandler -> IO ()
runViewHandler _ handler = handler `deepseq` mapM_ executeAction handler

#else

defineControllerView _ _ _ = ReactView (ReactViewRef ())

#endif

{-# NOINLINE defineControllerView #-}

---------------------------------------------------------------------------------------------------
--- Two versions of defineView
---------------------------------------------------------------------------------------------------

-- | A view is a re-usable component of the page which accepts properties of type @props@ from its
-- parent and re-renders itself whenever the properties change.
--
-- One option to implement views is to just use a Haskell function taking the @props@ as input and
-- producing a 'ReactElementM'.  For small views, such a Haskell function is ideal.
-- Using a 'ReactView' provides more than just a Haskell function when used with a key property with
-- 'viewWithSKey' and 'viewWithIKey'.  The key property allows React to more easily reconcile the virtual DOM with the
-- browser DOM.
--
-- The following is two example views: @mainSection_@ is just a Haskell function and @todoItem@
-- is a React view.  We use the convention that an underscore suffix signifies a combinator
-- which can be used in the rendering function.
--
-- >mainSection_ :: TodoState -> ReactElementM ViewEventHandler ()
-- >mainSection_ st = section_ ["id" $= "main"] $ do
-- >    input_ [ "id" $= "toggle-all"
-- >           , "type" $= "checkbox"
-- >           , "checked" $= if all (todoComplete . snd) $ todoList st then "checked" else ""
-- >           , onChange $ \_ -> dispatchTodo ToggleAllComplete
-- >           ]
-- >
-- >    label_ [ "htmlFor" $= "toggle-all"] "Mark all as complete"
-- >    ul_ [ "id" $= "todo-list" ] $ mapM_ todoItem_ $ todoList st
-- >
-- >todoItem :: ReactView (Int, Todo)
-- >todoItem = defineView "todo item" $ \(todoIdx, todo) ->
-- >    li_ [ classNames [("completed", todoComplete todo), ("editing", todoIsEditing todo)]
-- >        , "key" @= todoIdx
-- >        ] $ do
-- >
-- >        div_ [ "className" $= "view"] $ do
-- >            input_ [ "className" $= "toggle"
-- >                   , "type" $= "checkbox"
-- >                   , "checked" @= todoComplete todo
-- >                   , onChange $ \_ -> dispatchTodo $ TodoSetComplete todoIdx $ not $ todoComplete todo
-- >                   ]
-- >
-- >            label_ [ onDoubleClick $ \_ _ -> dispatchTodo $ TodoEdit todoIdx] $
-- >                elemText $ todoText todo
-- >
-- >            button_ [ "className" $= "destroy"
-- >                    , onClick $ \_ _ -> dispatchTodo $ TodoDelete todoIdx
-- >                    ] mempty
-- >
-- >        when (todoIsEditing todo) $
-- >            todoTextInput_ TextInputArgs
-- >                { tiaId = Nothing
-- >                , tiaClass = "edit"
-- >                , tiaPlaceholder = ""
-- >                , tiaOnSave = dispatchTodo . UpdateText todoIdx
-- >                , tiaValue = Just $ todoText todo
-- >                }
-- >
-- >todoItem_ :: (Int, Todo) -> ReactElementM eventHandler ()
-- >todoItem_ !todo = viewWithIKey todoItem (fst todo) todo mempty
defineView :: Typeable props
       => JSString -- ^ A name for this view, used only for debugging/console logging
       -> (props -> ReactElementM ViewEventHandler ()) -- ^ The rendering function
       -> ReactView props

#ifdef __GHCJS__

defineView name buildNode = unsafePerformIO $ do
    let render () props = return $ buildNode props
    renderCb <- mkRenderCallback (const $ return ()) runViewHandler render
    ReactView <$> js_createView name renderCb

#else

defineView _ _ = ReactView (ReactViewRef ())

#endif

{-# NOINLINE defineView #-}

---------------------------------------------------------------------------------------------------
--- Two versions of defineStatefulView
---------------------------------------------------------------------------------------------------

-- | A stateful-view event handler produces a list of store actions and potentially a new state.  If
-- the new state is nothing, no change is made to the state (which allows an optimization in that we
-- do not need to re-render the view).
--
-- Changing the state causes a re-render which will cause a new event handler to be created.  If the
-- handler closes over the state passed into the rendering function, there is a race if multiple
-- events occur before React causes a re-render.  Therefore, the handler takes the current state as
-- input.  Your handlers therefore should ignore the state passed into the render function and
-- instead use the state passed directly to the handler.
type StatefulViewEventHandler state = state -> ([SomeStoreAction], Maybe state)

-- | Change the event handler from 'ViewEventHandler' to 'StatefulViewEventHandler' to allow you to embed
-- combinators with 'ViewEventHandler's into a stateful view.  Each such lifted handler makes no change to
-- the state.
liftViewToStateHandler :: ReactElementM ViewEventHandler a -> ReactElementM (StatefulViewEventHandler st) a
liftViewToStateHandler = transHandler (\h _ -> (h, Nothing))

-- | A stateful view is a re-usable component of the page which keeps track of internal state.
-- Try to keep as many views as possible stateless.  The React documentation on
-- <https://facebook.github.io/react/docs/interactivity-and-dynamic-uis.html interactivity and dynamic UIs>
-- has some discussion of what should and should not go into the state.
--
-- The rendering function is a pure function of the state and the properties from the parent.  The
-- view will be re-rendered whenever the state or properties change.  The only way to
-- transform the internal state of the view is via an event handler, which can optionally produce
-- new state.  Any more complicated state should be moved out into a (possibly new) store.
--
-- >data TextInputArgs = TextInputArgs {
-- >      tiaId :: Maybe JSString
-- >    , tiaClass :: JSString
-- >    , tiaPlaceholder :: JSString
-- >    , tiaOnSave :: Text -> [SomeStoreAction]
-- >    , tiaValue :: Maybe Text
-- >} deriving (Typeable)
-- >
-- >todoTextInput :: ReactView TextInputArgs
-- >todoTextInput = defineStatefulView "todo text input" "" $ \curText args ->
-- >    input_ $
-- >        maybe [] (\i -> ["id" &= i]) (tiaId args)
-- >        ++
-- >        [ "className" &= tiaClass args
-- >        , "placeholder" &= tiaPlaceholder args
-- >        , "value" &= curText
-- >        , "autoFocus" &= True
-- >        , onChange $ \evt _ -> ([], Just $ target evt "value")
-- >        , onBlur $ \_ _ curState ->
-- >             if not (Text.null curState)
-- >                 then (tiaOnSave args curState, Just "")
-- >                 else ([], Nothing)
-- >        , onKeyDown $ \_ evt curState ->
-- >             if keyCode evt == 13 && not (Text.null curState) -- 13 is enter
-- >                 then (tiaOnSave args curState, Just "")
-- >                 else ([], Nothing)
-- >        ]
-- >
-- >todoTextInput_ :: TextInputArgs -> ReactElementM eventHandler ()
-- >todoTextInput_ !args = view todoTextInput args mempty
defineStatefulView :: (Typeable state, NFData state, Typeable props)
               => JSString -- ^ A name for this view, used only for debugging/console logging
               -> state -- ^ The initial state
               -> (state -> props -> ReactElementM (StatefulViewEventHandler state) ()) -- ^ The rendering function
               -> ReactView props

#ifdef __GHCJS__

defineStatefulView name initial buildNode = unsafePerformIO $ do
    initialRef <- export initial
    let render state props = return $ buildNode state props
    renderCb <- mkRenderCallback (js_ReactGetState >=> parseExport) runStateViewHandler render
    ReactView <$> js_createStatefulView name initialRef renderCb

-- | Transform a stateful view event handler to a raw event handler
runStateViewHandler :: (Typeable state, NFData state)
                    => ReactThis state props -> StatefulViewEventHandler state -> IO ()
runStateViewHandler this handler = do
    st <- js_ReactGetState this >>= parseExport

    let (actions, mNewState) = handler st

    case mNewState of
        Nothing -> return ()
        Just newState -> do
            newStateRef <- newState `deepseq` export newState
            js_ReactUpdateAndReleaseState this newStateRef

    -- nothing above here should block, so the handler callback should still be running syncronous,
    -- so the deepseq of actions should still pick up the proper event object.
    actions `deepseq` mapM_ executeAction actions

#else

defineStatefulView _ _ _ = ReactView (ReactViewRef ())

#endif

{-# NOINLINE defineStatefulView #-}

---------------------------------------------------------------------------------------------------
--- Class
---------------------------------------------------------------------------------------------------

---------------------------------------------------------------------------------------------------
--- Various GHCJS only utilities
---------------------------------------------------------------------------------------------------

#ifdef __GHCJS__

foreign import javascript unsafe
    "$1['state'].hs"
    js_ReactGetState :: ReactThis state props -> IO (Export state)

foreign import javascript unsafe
    "$1['props'].hs"
    js_ReactGetProps :: ReactThis state props -> IO (Export props)

foreign import javascript unsafe
    "$1._updateAndReleaseState($2)"
    js_ReactUpdateAndReleaseState :: ReactThis state props -> Export state -> IO ()

-- React 0.13 has React.findDOMNode, while 0.14 moves it to ReactDOM.findDOMNode.  Also, 0.14
-- does not need to call findDOMNode on refs.

foreign import javascript unsafe
    "typeof ReactDOM === 'object' ? ReactDOM['findDOMNode']($1) : React['findDOMNode']($1)"
    js_ReactFindDOMNode :: ReactThis state props -> IO JSVal

foreign import javascript unsafe
    "typeof ReactDOM === 'object' ? $1['refs'][$2] : React['findDOMNode']($1['refs'][$2])"
    js_ReactGetRef :: ReactThis state props -> JSString -> IO JSVal

newtype RenderCbArg = RenderCbArg JSVal
instance IsJSVal RenderCbArg

foreign import javascript unsafe
    "$1.newCallbacks = $2; $1.elem = $3;"
    js_RenderCbSetResults :: RenderCbArg -> JSVal -> ReactElementRef -> IO ()

foreign import javascript unsafe
    "hsreact$mk_ctrl_view($1, $2, $3)"
    js_createControllerView :: JSString
                            -> ReactStoreRef storeData
                            -> Callback (JSVal -> JSVal -> IO ())
                            -> IO (ReactViewRef props)

-- | Create a view with no state.
foreign import javascript unsafe
    "hsreact$mk_view($1, $2)"
    js_createView :: JSString
                  -> Callback (JSVal -> JSVal -> IO ())
                  -> IO (ReactViewRef props)

-- | Create a view which tracks its own state.  Similar releasing needs to happen for callbacks and
-- properties as for controller views.
foreign import javascript unsafe
    "hsreact$mk_stateful_view($1, $2, $3)"
    js_createStatefulView :: JSString
                          -> Export state
                          -> Callback (JSVal -> JSVal -> IO ())
                          -> IO (ReactViewRef props)

foreign import javascript unsafe
    "hsreact$mk_lifecycle_view($1, $2, $3, $4, $5, $6, $7, $8, $9)"
    js_makeLifecycleView :: JSString -> Export state -> Callback (JSVal -> JSVal -> IO ())
                         -> JSVal -> JSVal -> JSVal -> JSVal -> JSVal -> JSVal -> IO (ReactViewRef props)

mkRenderCallback :: Typeable props
                 => (ReactThis state props -> IO state) -- ^ parse state
                 -> (ReactThis state props -> eventHandler -> IO ()) -- ^ execute event args
                 -> (state -> props -> IO (ReactElementM eventHandler ())) -- ^ renderer
                 -> IO (Callback (JSVal -> JSVal -> IO ()))
mkRenderCallback parseState runHandler render = syncCallback2 ContinueAsync $ \thisRef argRef -> do
    let this = ReactThis thisRef
        arg = RenderCbArg argRef
    state <- parseState this
    props <- js_ReactGetProps this >>= parseExport
    node <- render state props
    (element, evtCallbacks) <- mkReactElement (runHandler this) this node

    evtCallbacksRef <- toJSVal evtCallbacks
    js_RenderCbSetResults arg evtCallbacksRef element

parseExport :: Typeable a => Export a -> IO a
parseExport a = do
    mdata <- derefExport a
    maybe (error "Unable to load export from javascript") return mdata

#endif


----------------------------------------------------------------------------------------------------
--- Element creation for views
----------------------------------------------------------------------------------------------------


-- | Create an element from a view.  I suggest you make a combinator for each of your views, similar
-- to the examples above such as @todoItem_@.
view :: Typeable props
     => ReactView props -- ^ the view
     -> props -- ^ the properties to pass into the instance of this view
     -> ReactElementM eventHandler a -- ^ The children of the element
     -> ReactElementM eventHandler a
view rc props (ReactElementM child) =
    let (a, childEl) = runWriter child
     in elementToM a $ ViewElement (reactView rc) Nothing props childEl

-- | Keys in React can either be strings or integers
class ReactViewKey key where
    toKeyRef :: key -> JSVal
#if __GHCJS__
instance ReactViewKey String where
    toKeyRef = pToJSVal

instance ReactViewKey Int where
    toKeyRef = pToJSVal
#else
instance ReactViewKey String where
    toKeyRef = const ()

instance ReactViewKey Int where
    toKeyRef = const ()
#endif

-- | A deprecated way to create a view with a key which has problems when OverloadedStrings is
-- active.  Use 'viewWithSKey' or 'viewWithIKey' instead.
viewWithKey :: (Typeable props, ReactViewKey key)
            => ReactView props -- ^ the view
            -> key -- ^ A value unique within the siblings of this element
            -> props -- ^ The properties to pass to the view instance
            -> ReactElementM eventHandler a -- ^ The children of the view
            -> ReactElementM eventHandler a
viewWithKey rc key props (ReactElementM child) =
    let (a, childEl) = runWriter child
     in elementToM a $ ViewElement (reactView rc) (Just $ toKeyRef key) props childEl

-- | Create an element from a view, and also pass in a string key property for the instance.  Key
-- properties speed up the <https://facebook.github.io/react/docs/reconciliation.html reconciliation>
-- of the virtual DOM with the DOM.  The key does not need to be globally unqiue, it only needs to
-- be unique within the siblings of an element.
viewWithSKey :: Typeable props
             => ReactView props -- ^ the view
             -> JSString -- ^ The key, a value unique within the siblings of this element
             -> props -- ^ The properties to pass to the view instance
             -> ReactElementM eventHandler a -- ^ The children of the view
             -> ReactElementM eventHandler a
viewWithSKey rc key props (ReactElementM child) =
    let (a, childEl) = runWriter child
     in elementToM a $ ViewElement (reactView rc) (Just $ pToJSVal key) props childEl

-- | Similar to 'viewWithSKey', but with an integer key instead of a string key.
viewWithIKey :: Typeable props
             => ReactView props -- ^ the view
             -> Int -- ^ The key, a value unique within the siblings of this element
             -> props -- ^ The properties to pass to the view instance
             -> ReactElementM eventHandler a -- ^ The children of the view
             -> ReactElementM eventHandler a
viewWithIKey rc key props (ReactElementM child) =
    let (a, childEl) = runWriter child
     in elementToM a $ ViewElement (reactView rc) (Just $ pToJSVal key) props childEl

-- | Create a 'ReactElement' for a class defined in javascript.  See
-- 'React.Flux.Combinators.foreign_' for a convenient wrapper and some examples.
foreignClass :: JSVal -- ^ The javascript reference to the class
             -> [PropertyOrHandler eventHandler] -- ^ properties and handlers to pass when creating an instance of this class.
             -> ReactElementM eventHandler a -- ^ The child element or elements
             -> ReactElementM eventHandler a

#if __GHCJS__

foreignClass name attrs (ReactElementM child) =
    let (a, childEl) = runWriter child
     in elementToM a $ ForeignElement (Right $ ReactViewRef name) attrs childEl

#else
foreignClass _ _ x = x
#endif

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

#ifdef __GHCJS__

rawJsRendering trans (ReactElementM child) =
    let (a, childEl) = runWriter child
        trans' thisVal childLst =
          ReactElementRef <$> trans thisVal (JSA.fromList $ map reactElementRef childLst)
     in elementToM a $ RawJsElement trans' childEl

#else

rawJsRendering _ x = x

#endif

-- | A class which is used to implement <https://wiki.haskell.org/Varargs variable argument functions>.
-- These variable argument functions are used to convert from a JavaScript
-- <https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Functions/arguments arguments array>
-- to a Haskell value of type @props@.
--
-- Any function where each argument implements 'FromJSVal' and the result is 'ReturnProps' is an
-- instance of this class.  Entries from the JavaScript arguments array are matched one-by-one to
-- the arguments before 'ReturnProps' value.  If the Haskell function has more parameters than the
-- javascript @arguments@ object, a javascript null is used for the conversion.  Since the 'Maybe'
-- instance of 'FromJSVal' converts null references to 'Nothing', you can exploit this to handle
-- arguments not given to the JavaScript function.
class ArgumentsToProps props a | a -> props where
    returnViewFromArguments :: JSArray -> Int -> a -> IO props

-- | A type needed to make GHC happy when solving for instances of 'ArgumentsToProps'.
newtype ReturnProps props = ReturnProps props

instance ArgumentsToProps props (ReturnProps props) where
    returnViewFromArguments _ _ (ReturnProps v) = return v

instance (FromJSVal a, ArgumentsToProps props b) => ArgumentsToProps props (a -> b) where
#if __GHCJS__
    returnViewFromArguments args k f = do
        ma <- fromJSVal $ if k >= JSA.length args then nullRef else JSA.index k args
        a <- maybe (error "Unable to decode callback argument") return ma
        returnViewFromArguments args (k+1) $ f a
#else
    returnViewFromArguments _ _ _ = error "Not supported in GHC"
#endif

-- | Export a Haskell view to a JavaScript function.  This allows you to embed a Haskell react-flux
-- application into a larger existing JavaScript React application.  If you want to use JavaScript
-- classes in your Haskell application, you should instead use 'React.Flux.Combinators.foreign_' and 'foreignClass'.
--
-- The way this works is as follows:
--
-- 1. You create a Haskell function which translates the javascript arguments of into a Haskell
-- value of type @ReturnProps props@.  This is a variable-argument function using the 'ArgumentsToProps' class.
-- For example,
--
--       @
--       data MyProps = MyProps { theInt :: Int, theString :: String }
--       myArgsToProps :: Int -> String -> ReturnProps MyProps
--       myArgsToProps i s = ReturnProps $ MyProps i s
--       @
--
-- 2. You create a view which receives these properties and renders itself.  This view will not
-- receive any children.
--
--       @
--       myView :: ReactView MyProps
--       myView = defineView "my view" $ \\myProps -> ...
--       @
--
-- 3. You can then use 'exportViewToJavaScript' to create a JavaScript function.  When this
-- JavaScript function is executed, the JavaScript arguments are converted to the props,
-- the view is rendered using the props, and the resulting React element is returned from the
-- JavaScript function.
--
--       @
--       foreign import javascript unsafe
--           "window[\'myHaskellView\'] = $1;"
--           js_setMyView :: JSVal -> IO ()
--
--       exportMyView :: IO ()
--       exportMyView = exportViewToJavaScript myView myArgsToProps >>= js_setMyView
--       @
--
--       @exportMyView@ should be called from your main function.  After executing @exportMyView@,
--       the @window.myHaskellView@ property will be a javascript function.
--
-- 4. Call the javascript function with two arguments to return a React element which can be used
-- in a JavaScript React class rendering function.
-- 
--       @
--       var myJsView = React.createClass({
--           render: function() {
--               return \<div\>{window.myHaskellView(5, "Hello World")}\</div\>;
--           }
--       };
--       @
exportViewToJavaScript :: (Typeable props, ArgumentsToProps props func) => ReactView props -> func -> IO JSVal
exportViewToJavaScript v func = do
    (_callbackToRelease, wrappedCb) <- exportViewToJs (reactView v) (\arr -> returnViewFromArguments arr 0 func)
    return wrappedCb






newtype View (props :: [*]) = View (ReactViewRef ())

type family ViewPropsToElement (props :: [*]) (handler :: *) where
  ViewPropsToElement '[] handler = ReactElementM handler ()
  ViewPropsToElement (a ': rest) handler = a -> ViewPropsToElement rest handler

type family ViewPropsToRender (props :: [*]) where
  ViewPropsToRender '[] = IO ()
  ViewPropsToRender (a ': rest) = a -> ViewPropsToRender rest

class ViewProps (props :: [*]) where
  viewPropsToJs :: Proxy props -> Proxy handler -> ReactViewRef () -> JSString -> (NewJsProps -> IO NewJsProps) -> ViewPropsToElement props handler
  renderViewProps :: Proxy props -> ReactViewRef () -> JSString -> (NewJsProps -> IO NewJsProps) -> ViewPropsToRender props
  applyViewPropsFromJs :: Proxy props -> ViewPropsToElement props handler -> NewJsProps -> Int -> IO (ReactElementM handler ())

class ExportViewProps (props :: [*]) where
  applyViewPropsFromArray :: Proxy props -> JSArray -> Int -> NewJsProps -> IO NewJsProps

instance ViewProps '[] where
  viewPropsToJs _ _ ref k props = elementToM () $ NewViewElement ref k props

#ifdef __GHCJS__
  renderViewProps _ ref k props =
    do (e, _) <- mkReactElement id (ReactThis nullRef) $ elementToM () $ NewViewElement ref k props
       js_ReactRender e k
#else
  renderViewProps _ _ _ _ = return ()
#endif

  applyViewPropsFromJs _ x _ _ = return x

instance ExportViewProps '[] where
  applyViewPropsFromArray _ _ _ p = return p

instance (ViewProps rest, Typeable a) => ViewProps (a ': (rest :: [*])) where
  viewPropsToJs _ h ref k props = \a -> viewPropsToJs (Proxy :: Proxy rest) h ref k (props >=> pushProp a)

  renderViewProps _ ref k props = \a -> a `seq` renderViewProps (Proxy :: Proxy rest) ref k (props >=> pushProp a)

  applyViewPropsFromJs _ f props i = do
    val <- getProp props i
    applyViewPropsFromJs (Proxy :: Proxy rest) (f val) props (i+1)

instance forall (rest :: [*]) a. (ExportViewProps rest, Typeable a, FromJSVal a) => ExportViewProps (a ': rest) where
#ifdef __GHCJS__
  applyViewPropsFromArray _ inputArr k outputArr =
    do ma <- fromJSVal $ if k >= JSA.length inputArr then nullRef else JSA.index k inputArr
       a :: a <- maybe (error "Unable to decode callback argument") return ma
       outputArr' <- pushProp a outputArr
       applyViewPropsFromArray (Proxy :: Proxy rest) inputArr (k+1) outputArr'
#else
  applyViewPropsFromArray _ _ _ p = return p
#endif

mkView :: forall (props :: [*]). ViewProps props => JSString -> ViewPropsToElement props ViewEventHandler -> View props
#ifdef __GHCJS__
mkView name buildNode = unsafePerformIO $ do
  renderCb <- syncCallback2 ContinueAsync $ \thisRef argRef -> do
    let this = ReactThis thisRef
        arg = RenderCbArg argRef
    props <- js_PropsList this
    node <- applyViewPropsFromJs (Proxy :: Proxy props) buildNode props 0
    (element, evtCallbacks) <- mkReactElement (runViewHandler this) this node
    evtCallbacksRef <- toJSVal evtCallbacks
    js_RenderCbSetResults arg evtCallbacksRef element

  View <$> js_createNewView name renderCb
#else
mkView _ _ = View (ReactViewRef ())
#endif

exportReactViewToJavaScript :: forall (props :: [*]). (ExportViewProps props) => View props -> IO JSVal
#ifdef __GHCJS__
exportReactViewToJavaScript (View v) = do
    (_callbackToRelease, wrappedCb) <- exportNewViewToJs v $ \arr -> do
      emptyLst <- js_newEmptyPropList
      applyViewPropsFromArray (Proxy :: Proxy props) arr 0 emptyLst
    return wrappedCb

foreign import javascript unsafe
    "[]"
    js_newEmptyPropList :: IO NewJsProps
#else
exportReactViewToJavaScript _ = return ()
#endif

mkStatefulView :: forall (state :: *) (props :: [*]). (Typeable state, NFData state, ViewProps props)
               => JSString -- ^ A name for this view, used only for debugging/console logging
               -> state -- ^ The initial state
               -> (state -> ViewPropsToElement props (StatefulViewEventHandler state))
               -> View props

#ifdef __GHCJS__

mkStatefulView name initial buildNode = unsafePerformIO $ do
    initialRef <- export initial
    renderCb <- syncCallback2 ContinueAsync $ \thisRef argRef -> do
      let this = ReactThis thisRef
          arg = RenderCbArg argRef
      props <- js_PropsList this
      state <- js_ReactGetState this >>= parseExport
      node <- applyViewPropsFromJs (Proxy :: Proxy props) (buildNode state) props 0
      (element, evtCallbacks) <- mkReactElement (runStateViewHandler this) this node
      evtCallbacksRef <- toJSVal evtCallbacks
      js_RenderCbSetResults arg evtCallbacksRef element

    View <$> js_createNewStatefulView name initialRef renderCb

#else
mkStatefulView _ _ _ = View (ReactViewRef ())
#endif

view_ :: forall props handler. ViewProps (props :: [*]) => View props -> JSString -> ViewPropsToElement props handler
view_ (View ref) key = viewPropsToJs (Proxy :: Proxy props) (Proxy :: Proxy handler) ref key return

#ifdef __GHCJS__
foreign import javascript unsafe
  "$1['props'].hs"
  js_PropsList :: ReactThis state props -> IO NewJsProps

foreign import javascript unsafe
  "$1['state'].hs"
  js_NewStateDict :: ReactThis state props -> IO JsState

foreign import javascript unsafe
  "$1[$2]"
  js_getPropFromList :: NewJsProps -> Int -> IO (Export a)

foreign import javascript unsafe
  "$1.push($2)"
  js_pushProp :: NewJsProps -> Export a -> IO ()

foreign import javascript unsafe
    "hsreact$mk_new_view($1, $2)"
    js_createNewView :: JSString -> Callback (JSVal -> JSVal -> IO ()) -> IO (ReactViewRef props)

foreign import javascript unsafe
    "hsreact$mk_new_stateful_view($1, $2, $3)"
    js_createNewStatefulView :: JSString -> Export state -> Callback (JSVal -> JSVal -> IO ()) -> IO (ReactViewRef props)

foreign import javascript unsafe
    "(typeof ReactDOM === 'object' ? ReactDOM : React)['render']($1, document.getElementById($2))"
    js_ReactRender :: ReactElementRef -> JSString -> IO ()
#endif

getProp :: Typeable a => NewJsProps -> Int -> IO a
#ifdef __GHCJS__
getProp p i = js_getPropFromList p i >>= parseExport
#else
getProp = error "Not definfed"
#endif

pushProp :: Typeable a => a -> NewJsProps -> IO NewJsProps
#ifdef __GHCJS__
pushProp val props = do
  valE <- export val -- this will be released in the lifecycle callbacks of the class
  js_pushProp props valE
  return props
#else
pushProp _ = return
#endif









newtype JsState = JsState JSVal -- javascript map from store typerep fingerprint to value
data StoreArg store
data StoreField store (field :: k) a

type family ControllerViewToElement (stores :: [*]) (props :: [*]) (handler :: *) where
  ControllerViewToElement '[] props handler = ViewPropsToElement props handler
  ControllerViewToElement (StoreArg store ': rest) props handler = store -> ControllerViewToElement rest props handler
  ControllerViewToElement (StoreField store field a ': rest) props handler = a -> ControllerViewToElement rest props handler

class ControllerViewStores (stores :: [*]) where
  applyControllerViewFromJs :: forall props handler. ViewProps props
                            => Proxy stores
                            -> Proxy props
                            -> ControllerViewToElement stores props handler
                            -> JsState
                            -> NewJsProps
                            -> Int
                            -> IO (ReactElementM handler ())
  stateForView :: Proxy stores -> Int -> M.HashMap TypeRep [StoreToState]


instance ControllerViewStores '[] where
  applyControllerViewFromJs _ p f _ props _ = applyViewPropsFromJs p f props 0
  stateForView _ _ = mempty

data StoreToState = StoreState Int
                  | StoreDerivedState
                    { storeDerivedOffset :: Int
                    , storeToStateCallback :: IO (Callback (JSVal -> IO ()))
                    }


class HasField (x :: k) r a | x r -> a where
  getField :: r -> a

--class DerivedFromStore store a | store -> a, a -> store where
--  type IntermediateValue store a
--  extractValue :: store -> IntermediateValue store a
--  deriveFromValue :: IntermediateValue store a -> a

findFromState :: Typeable a => Int -> JsState -> IO a
#ifdef __GHCJS__
findFromState i s = js_findFromState i s >>= unsafeDerefExport

foreign import javascript unsafe
  "$2[$1]"
  js_findFromState :: Int -> JsState -> IO (Export a)

#else
findFromState _ _ = error "Can't use views from GHC, only GHCJS"
#endif

instance (ControllerViewStores rest, StoreData store, Typeable store)
   => ControllerViewStores (StoreArg store ': (rest :: [*])) where
  applyControllerViewFromJs _ p f st props i = do
    sval <- findFromState i st
    applyControllerViewFromJs (Proxy :: Proxy rest) p (f sval) st props (i+1)

  stateForView _ i = M.insertWith (++) storeT [StoreState i] (stateForView (Proxy :: Proxy rest) (i+1))
    where
      storeT = typeRep (Proxy :: Proxy store)

instance (ControllerViewStores rest, StoreData store, HasField field store a, Typeable store, Typeable a)
   => ControllerViewStores (StoreField store field a ': (rest :: [*])) where
  applyControllerViewFromJs _ p f st props i = do
    sval <- findFromState i st
    applyControllerViewFromJs (Proxy :: Proxy rest) p (f sval) st props (i+1)

  stateForView _ i = M.insertWith (++) storeT [StoreDerivedState i derive] (stateForView (Proxy :: Proxy rest) (i+1))
    where
      storeT = typeRep (Proxy :: Proxy store)
      derive =
#ifdef __GHCJS__
        syncCallback1 ThrowWouldBlock $ \arg -> do
          mstoreD :: Maybe store <- js_getDeriveInput arg >>= derefExport
          storeD <- maybe (error "Can't decode store") return mstoreD
          let a :: a = getField @field storeD
          aE <- a `seq` unsafeExport a
          js_setDeriveOutput arg aE

foreign import javascript unsafe
  "$1.input"
  js_getDeriveInput :: JSVal -> IO (Export storeData)

foreign import javascript unsafe
  "$1.output = $2"
  js_setDeriveOutput :: JSVal -> Export a -> IO ()
#else
        return ()
#endif

mkControllerView :: forall (stores :: [*]) (props :: [*]). (ViewProps props, ControllerViewStores stores)
                 => JSString -> ControllerViewToElement stores props ViewEventHandler -> View props
#ifdef __GHCJS__
mkControllerView name buildNode = unsafePerformIO $ do
  renderCb <- syncCallback2 ContinueAsync $ \thisRef argRef -> do
    let this = ReactThis thisRef
        arg = RenderCbArg argRef
    props <- js_PropsList this
    st <- js_NewStateDict this
    node <- applyControllerViewFromJs (Proxy :: Proxy stores) (Proxy :: Proxy props) buildNode st props 0
    (element, evtCallbacks) <- mkReactElement (runViewHandler this) this node
    evtCallbacksRef <- toJSVal evtCallbacks
    js_RenderCbSetResults arg evtCallbacksRef element

  artifacts <- js_emptyArtifacts
  forM_ (M.toList $ stateForView (Proxy :: Proxy stores) 0) $ \(ty, states) -> do
    art <- js_newArtifact
    forM_ states $ \s ->
      case s of
        StoreState i -> js_addStoreState art i
        StoreDerivedState i mkCall -> do
          callback <- mkCall
          js_addStoreDerivedState art i callback
    js_setArtifact artifacts (typeJsKey ty) art

  View <$> js_createNewCtrlView name renderCb artifacts


newtype Artifacts = Artifacts JSVal
instance IsJSVal Artifacts

newtype Artifact = Artifact JSVal
instance IsJSVal Artifact

foreign import javascript unsafe
  "{}"
  js_emptyArtifacts :: IO Artifacts

foreign import javascript unsafe
  "[]"
  js_newArtifact :: IO Artifact

foreign import javascript unsafe
  "$1.push({i: $2})"
  js_addStoreState :: Artifact -> Int -> IO ()

foreign import javascript unsafe
  "$1.push({i: $2, call: $3})"
  js_addStoreDerivedState :: Artifact -> Int -> Callback (JSVal -> IO ()) -> IO ()

foreign import javascript unsafe
  "$1[$2] = $3"
  js_setArtifact :: Artifacts -> JSString -> Artifact -> IO ()

foreign import javascript unsafe
  "hsreact$mk_new_ctrl_view($1, $2, $3)"
  js_createNewCtrlView :: JSString -> Callback (JSVal -> JSVal -> IO ()) -> Artifacts -> IO (ReactViewRef props)
#else
mkControllerView _ _ = View (ReactViewRef ())
#endif
