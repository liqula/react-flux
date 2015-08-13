-- | Internal module containing the view definitions
module React.Flux.Views (
    ReactView(..)
  , defineControllerView
  , defineView
  , ViewEventHandler
  , defineStatefulView
  , StatefulViewEventHandler
  , LPropsAndState(..)
  , LDOM(..)
  , LSetStateFn
  , LifecycleViewConfig(..)
  , lifecycleConfig
  , lifecycleView
  , view
  , viewWithKey
  , foreignClass
) where

import Control.DeepSeq
import Control.Monad.Writer
import Data.Typeable (Typeable)
import System.IO.Unsafe (unsafePerformIO)

import React.Flux.Store
import React.Flux.Internal
import React.Flux.Export
import React.Flux.DOM (div_)

#ifdef __GHCJS__
import GHCJS.Types (JSRef, castRef, JSFun, JSString)
import GHCJS.Foreign (syncCallback1, syncCallback2, toJSString, ForeignRetention(..), jsNull)
import GHCJS.Marshal (ToJSRef(..))
#endif

type Callback a = JSFun a

-- | A view is conceptually a function from @props@ to a tree of elements.
-- The function receives a value of type @props@ from its parent in the virtual DOM.
-- Additionally, the rendering function can depend on some internal state or store data.  Based on the @props@ and
-- the internal state, the rendering function produces a virtual tree of elements which React then
-- reconciles with the browser DOM.
--
-- This module supports 3 kinds of views.
--
-- * Controller View, created by 'defineControllerView'.  A controller view provides the glue code
-- between a store and the view, and as such is a pure function taking as input the store data and
-- the properties and producing a tree of elements.  No internal state is allowed.
--
-- * View.  A view is pure function from props to a tree of elements which does not maintain any
-- internal state.  It can eiter be modeled by
-- just a Haskell function without a 'ReactView', or as a 'ReactView' created by 'defineView'.  Using
-- the machinery of a 'ReactView' is helpful because it allows React to more easily reconcile the
-- virtual DOM with the DOM and leads to faster rendering (as long as you use 'viewWithKey'
-- when creating an instance of the view).
--
-- * Stateful View, created by 'defineStatefulView'.  A stateful view keeps track of
-- some internal state.  It
-- consists of a pure function taking as input the properties and current state and producing a tree
-- of elements.  Event handlers registered on elements can transform the state and produce actions,
-- but cannot perform any other @IO@.
--
-- All of the views provided by this module are pure, in the sense that the rendering function and
-- event handlers cannot perform any IO.  All IO occurs inside the 'transform' function of a store.
newtype ReactView props = ReactView { reactView :: ReactViewRef props }

---------------------------------------------------------------------------------------------------
--- Two versions of defineControllerView
---------------------------------------------------------------------------------------------------

-- | Event handlers in a controller-view and a view transform events into actions, but are not
-- allowed to perform any 'IO'.
type ViewEventHandler = [SomeStoreAction]

-- | A controller view provides the glue between a 'ReactStore' and the DOM.
--
-- The controller-view registers with the given store.  Whenever the store is transformed, the
-- controller-view re-renders itself.  It is recommended to have one controller-view for each
-- significant section of the page.  Controller-views deeper in the page tree can cause complexity
-- because data is now flowing into the page in multiple possibly conflicting places.  You must
-- balance the gain of encapsulated components versus the complexity of multiple entry points for
-- data into the page.  Note that multiple controller views can register with the same store.
--
-- Each instance of a controller-view also accepts properties of type @props@ from its parent.
-- Whenever the parent re-renders itself, the new properties will be passed down to the
-- controller-view causing it to re-render itself.
--
-- Events registered on controller-views just produce actions, which get dispatched to the
-- appropriate store which causes the store to transform itself, which eventually leads to the
-- controller-view re-rendering.  This one-way flow of data from actions to store to
-- controller-views is central to the flux design.
--
-- While the above re-rendering on any store data or property change is conceptually what occurs,
-- React uses a process of <https://facebook.github.io/react/docs/reconciliation.html reconciliation>
-- to speed up re-rendering.  The best way of taking advantage of reconciliation is to
-- use key properties with 'viewWithKey'.
--
-- >todoApp :: ReactView ()
-- >todoApp = defineControllerView "todo app" todoStore $ \todoState () ->
-- >    div_ $ do
-- >        todoHeader_
-- >        mainSection_ todoState
-- >        todoFooter_ todoState
defineControllerView :: (StoreData storeData, Typeable props)
                 => String -- ^ A name for this view
                 -> ReactStore storeData -- ^ The store this controller view should attach to.
                 -> (storeData -> props -> ReactElementM ViewEventHandler ()) -- ^ The rendering function
                 -> ReactView props

#ifdef __GHCJS__

defineControllerView name (ReactStore store _) buildNode = unsafePerformIO $ do
    let render sd props = return $ buildNode sd props
    renderCb <- mkRenderCallback parseExport runViewHandler render
    ReactView <$> js_createControllerView (toJSString name) store renderCb

-- | Transform a controller view handler to a raw handler.
runViewHandler :: RenderCbArgs state props -> ViewEventHandler -> IO ()
runViewHandler _ handler = handler `deepseq` mapM_ dispatchSomeAction handler

#else

defineControllerView _ _ _ = ReactView (ReactViewRef ())

#endif

{-# NOINLINE defineControllerView #-}

---------------------------------------------------------------------------------------------------
--- Two versions of defineView
---------------------------------------------------------------------------------------------------

-- | A view is a re-usable component of the page which does not track any state itself.
-- Each instance of a view accepts properties of type @props@ from its parent and re-renders itself
-- whenever the properties change.
--
-- One option to implement views is to just use a Haskell function taking the @props@ as input and
-- producing a 'ReactElementM'.  For small views, such a Haskell function is ideal.
-- Using a view provides more than just a Haskell function when used with a key property with
-- 'viewWithKey'.  The key property allows React to more easily reconcile the virtual DOM with the
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
-- >           , onChange $ \_ -> [todoA ToggleAllComplete]
-- >           ]
-- >
-- >    label_ [ "htmlFor" $= "toggle-all"] "Mark all as complete"
-- >    ul_ [ "id" $= "todo-list" ] $ mapM_ todoItem_ $ todoList st
-- >
-- >todoItem :: ReactView (Int, Todo)
-- >todoItem = defineView "todo item" $ \(todoIdx, todo) ->
-- >    li_ [ "className" @= (intercalate "," ([ "completed" | todoComplete todo] ++ [ "editing" | todoIsEditing todo ]) :: String)
-- >        , "key" @= todoIdx
-- >        ] $ do
-- >        
-- >        div_ [ "className" $= "view"] $ do
-- >            input_ [ "className" $= "toggle"
-- >                   , "type" $= "checkbox"
-- >                   , "checked" @= todoComplete todo
-- >                   , onChange $ \_ -> [todoA $ TodoSetComplete todoIdx $ not $ todoComplete todo]
-- >                   ]
-- >
-- >            label_ [ onDoubleClick $ \_ _ -> [todoA $ TodoEdit todoIdx] ] $
-- >                text $ todoText todo
-- >
-- >            button_ [ "className" $= "destroy"
-- >                    , onClick $ \_ _ -> [todoA $ TodoDelete todoIdx]
-- >                    ]
-- >                    "Delete"
-- >
-- >        when (todoIsEditing todo) $
-- >            todoTextInput_ TextInputArgs
-- >                { tiaId = Nothing
-- >                , tiaClass = "edit"
-- >                , tiaPlaceholder = ""
-- >                , tiaOnSave = todoA . UpdateText todoIdx
-- >                , tiaValue = Just $ todoText todo
-- >                }
-- >
-- >todoItem_ :: (Int, Todo) -> ReactElementM eventHandler ()
-- >todoItem_ todo = viewWithKey todoItem (fst todo) todo mempty
defineView :: Typeable props
       => String -- ^ A name for this view
       -> (props -> ReactElementM ViewEventHandler ()) -- ^ The rendering function
       -> ReactView props

#ifdef __GHCJS__

defineView name buildNode = unsafePerformIO $ do
    let render () props = return $ buildNode props
    renderCb <- mkRenderCallback (const $ return ()) runViewHandler render
    ReactView <$> js_createView (toJSString name) renderCb

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

-- | A stateful view is a re-usable component of the page which keeps track of internal state.
--
-- The rendering function is a pure function of the state and the properties from the parent.  The
-- view will be re-rendered whenever the state or properties change.  The only way to
-- transform the internal state of the view is via an event handler, which can optionally produce
-- new state.  Any more complicated state should be moved out into a (possibly new) store.
--
-- >data TextInputArgs = TextInputArgs {
-- >      tiaId :: Maybe String
-- >    , tiaClass :: String
-- >    , tiaPlaceholder :: String
-- >    , tiaOnSave :: String -> SomeStoreAction
-- >    , tiaValue :: Maybe String
-- >} deriving (Typeable)
-- >
-- >todoTextInput :: ReactView TextInputArgs
-- >todoTextInput = defineStatefulView "todo text input" "" $ \curText args ->
-- >    input_ [ "className" @= tiaClass args
-- >           , "placeholder" @= tiaPlaceholder args
-- >           , "value" @= curText
-- >           , "autoFocus" @= True
-- >           , onBlur $ \_ _ curState -> ([tiaOnSave args curState | not $ null curState], Just "")
-- >           , onChange $ \evt _ -> ([], Just $ target evt "value")
-- >           , onKeyDown $ \_ evt curState ->
-- >                if keyCode evt == 13 && not (null curState) -- 13 is enter
-- >                    then ([tiaOnSave args curState], Just "")
-- >                    else ([], Nothing)
-- >           ]
-- >
-- >todoTextInput_ :: TextInputArgs -> ReactElementM eventHandler ()
-- >todoTextInput_ args = view todoTextInput args mempty
defineStatefulView :: (Typeable state, Typeable props)
               => String -- ^ A name for this view
               -> state -- ^ The initial state
               -> (state -> props -> ReactElementM (StatefulViewEventHandler state) ()) -- ^ The rendering function
               -> ReactView props

#ifdef __GHCJS__

defineStatefulView name initial buildNode = unsafePerformIO $ do
    initialRef <- export initial
    let render state props = return $ buildNode state props
    renderCb <- mkRenderCallback parseExport runStateViewHandler render
    ReactView <$> js_createStatefulView (toJSString name) initialRef renderCb

-- | Transform a stateful view event handler to a raw event handler
runStateViewHandler :: Typeable state
                    => RenderCbArgs state props -> StatefulViewEventHandler state -> IO ()
runStateViewHandler args handler = do
    alterState <- js_RenderCbRetrieveAlterStateFns args
    st <- parseExport =<< js_GetState alterState

    let (actions, mNewState) = handler st

    case mNewState of
        Nothing -> return ()
        Just newState -> do
            newStateRef <- export newState
            js_SetState alterState newStateRef

    -- nothing above here should block, so the handler callback should still be running syncronous,
    -- so the deepseq of actions should still pick up the proper event object.
    actions `deepseq` mapM_ dispatchSomeAction actions

#else

defineStatefulView _ _ _ = ReactView (ReactViewRef ())

#endif

{-# NOINLINE defineStatefulView #-}

---------------------------------------------------------------------------------------------------
--- Class
---------------------------------------------------------------------------------------------------

type HTMLElement = JSRef ()

data LPropsAndState props state = LPropsAndState
  { lGetProps :: IO props
  , lGetState :: IO state
  }

data LDOM = LDOM
  { lThis :: IO HTMLElement
  , lRef :: String -> IO HTMLElement
  }

type LSetStateFn state = state -> IO ()

data LifecycleViewConfig props state = LifecycleViewConfig
  { lRender :: state -> props -> ReactElementM (StatefulViewEventHandler state) ()
  , lComponentWillMount :: Maybe (LPropsAndState props state -> LSetStateFn state -> IO ())
  , lComponentDidMount :: Maybe (LPropsAndState props state -> LDOM -> LSetStateFn state -> IO ())
  , lComponentWillReceiveProps :: Maybe (LPropsAndState props state -> LDOM -> LSetStateFn state -> props -> IO ())
  , lComponentWillUpdate :: Maybe (LPropsAndState props state -> LDOM -> props -> state -> IO ())
  , lComponentDidUpdate :: Maybe (LPropsAndState props state -> LDOM -> LSetStateFn state -> props -> state -> IO ())
  , lComponentWillUnmount :: Maybe (LPropsAndState props state -> LDOM -> IO ())
  }

lifecycleConfig :: LifecycleViewConfig props state
lifecycleConfig = LifecycleViewConfig
    { lRender = \_ _ -> div_ mempty
    , lComponentWillMount = Nothing
    , lComponentDidMount = Nothing
    , lComponentWillReceiveProps = Nothing
    , lComponentWillUpdate = Nothing
    , lComponentDidUpdate = Nothing
    , lComponentWillUnmount = Nothing
    }

lifecycleView :: (Typeable props, Typeable state)
              => String -> state -> LifecycleViewConfig props state -> ReactView props

#ifdef __GHCJS__

lifecycleView name initialState cfg = unsafePerformIO $ do
    initialRef <- export initialState

    let render state props = return $ lRender cfg state props
    renderCb <- mkRenderCallback parseExport runStateViewHandler render

    let ps this = LPropsAndState { lGetProps = js_LgetProps this >>= parseExport
                                 , lGetState = js_LgetState this >>= parseExport
                                 }

        dom this = LDOM { lThis = js_ReactFindDOMNode this
                        , lRef = \r -> js_LgetRef this $ toJSString r
                        }

        setStateFn this s = export s >>= js_LupdateState this

    willMountCb <- mkLCallback (lComponentWillMount cfg) $ \f -> syncCallback1 AlwaysRetain False $ \this ->
        f (ps this) (setStateFn this)

    didMountCb <- mkLCallback (lComponentDidMount cfg) $ \f -> syncCallback1 AlwaysRetain False $ \this ->
        f (ps this) (dom this) (setStateFn this)

    willRecvPropsCb <- mkLCallback (lComponentWillReceiveProps cfg) $ \f -> syncCallback2 AlwaysRetain False $ \this newPropsE -> do
        newProps <- parseExport $ Export newPropsE
        f (ps this) (dom this) (setStateFn this) newProps

    willUpdateCb <- mkLCallback (lComponentWillUpdate cfg) $ \f -> syncCallback2 AlwaysRetain False $ \this arg -> do
        nextProps <- js_LgetProps arg >>= parseExport
        nextState <- js_LgetState arg >>= parseExport
        f (ps this) (dom this) nextProps nextState

    didUpdateCb <- mkLCallback (lComponentDidUpdate cfg) $ \f -> syncCallback2 AlwaysRetain False $ \this arg -> do
        oldProps <- js_LgetProps arg >>= parseExport
        oldState <- js_LgetState arg >>= parseExport
        f (ps this) (dom this) (setStateFn this) oldProps oldState

    willUnmountCb <- mkLCallback (lComponentWillUnmount cfg) $ \f -> syncCallback1 AlwaysRetain False $ \this ->
        f (ps this) (dom this)

    ReactView <$> js_makeLifecycleView (toJSString name) initialRef
                    renderCb willMountCb didMountCb willRecvPropsCb willUpdateCb didUpdateCb willUnmountCb

mkLCallback :: Maybe f -> (f -> IO (JSRef a)) -> IO (JSRef a)
mkLCallback Nothing _ = return jsNull
mkLCallback (Just f) c = c f 

foreign import javascript unsafe
    "React['findDOMNode']($1)"
    js_ReactFindDOMNode :: JSRef a -> IO (JSRef b)

foreign import javascript unsafe
    "$1['props'].hs"
    js_LgetProps :: JSRef a -> IO (Export props)

foreign import javascript unsafe
    "$1['state'].hs"
    js_LgetState :: JSRef a -> IO (Export state)

foreign import javascript unsafe
    "React['findDOMNode']($1['refs'][$2])"
    js_LgetRef :: JSRef a -> JSString -> IO (JSRef b)

foreign import javascript unsafe
    "$1.updateState($2)"
    js_LupdateState :: JSRef a -> Export state -> IO ()

#else

lifecycleView _ _ _ = ReactView ()

#endif

---------------------------------------------------------------------------------------------------
--- Various GHCJS only utilities
---------------------------------------------------------------------------------------------------

#ifdef __GHCJS__

-- | The view render callback is a haskell function given to the javascript object
-- that is called every time the class is to be rendered.  The argument to this callback is
-- javascript object used both for input and output.  The properties of this object are:
--
-- * state :: Export state
-- * props :: Export props
-- * newCallbacks :: [Callback].  This array is set by Haskell, and contains all the event
--       callbacks created as part of the rendering.  These callbacks will be stored and
--       after the next render, these callbacks will be freed.
-- * elem  This is set by Haskell and contains the value that should be returned by the render
--         function back to React.
--
--  In addition, for stateful views (not views or controller-views), there is one additional property @alterState@
--  which is used inside the event handlers.  @alterState@ has two properties, @setState@ and
--  @getState@.
newtype RenderCbArgs state props = RenderCbArgs (JSRef ())

foreign import javascript unsafe
    "$1.state"
    js_RenderCbRetrieveState :: RenderCbArgs state props -> IO (Export state)

foreign import javascript unsafe
    "$1.props"
    js_RenderCbRetrieveProps :: RenderCbArgs state props -> IO (Export props)

foreign import javascript unsafe
    "$1.newCallbacks = $2; $1.elem = $3;"
    js_RenderCbSetResults :: RenderCbArgs state props -> JSRef [Callback (JSRef () -> IO ())] -> ReactElementRef -> IO ()

newtype AlterStateFns state = AlterStateFns (JSRef ())

foreign import javascript unsafe
    "$1.alterState"
    js_RenderCbRetrieveAlterStateFns :: RenderCbArgs state props -> IO (AlterStateFns state)

foreign import javascript unsafe
    "$1.setState($2)"
    js_SetState :: AlterStateFns state -> Export state -> IO ()

foreign import javascript unsafe
    "$1.getState()"
    js_GetState :: AlterStateFns state -> IO (Export state)

foreign import javascript unsafe
    "hsreact$mk_ctrl_view($1, $2, $3)"
    js_createControllerView :: JSString
                            -> ReactStoreRef storeData
                            -> Callback (JSRef () -> IO ())
                            -> IO (ReactViewRef props)

-- | Create a view with no state.
foreign import javascript unsafe
    "hsreact$mk_view($1, $2)"
    js_createView :: JSString
                  -> Callback (JSRef () -> IO ())
                  -> IO (ReactViewRef props)

-- | Create a view which tracks its own state.  Similar releasing needs to happen for callbacks and
-- properties as for controller views.
foreign import javascript unsafe
    "hsreact$mk_stateful_view($1, $2, $3)"
    js_createStatefulView :: JSString
                          -> Export state
                          -> Callback (JSRef () -> IO ())
                          -> IO (ReactViewRef props)

foreign import javascript unsafe
    "hsreact$mk_lifecycle_view($1, $2, $3, $4, $5, $6, $7, $8, $9)"
    js_makeLifecycleView :: JSString -> Export state -> Callback (JSRef () -> IO ())
                         -> JSRef a -> JSRef b -> JSRef c -> JSRef d -> JSRef e -> JSRef f -> IO (ReactViewRef props)

mkRenderCallback :: Typeable props
                 => (Export state -> IO state) -- ^ parse state
                 -> (RenderCbArgs state props -> eventHandler -> IO ()) -- ^ execute event args
                 -> (state -> props -> IO (ReactElementM eventHandler ())) -- ^ renderer
                 -> IO (Callback (JSRef () -> IO ()))
mkRenderCallback parseState runHandler render = syncCallback1 AlwaysRetain False $ \argRef -> do
    let args = RenderCbArgs argRef
    stateE <- js_RenderCbRetrieveState args
    state <- parseState stateE

    propsE <- js_RenderCbRetrieveProps args
    mprops <- derefExport propsE
    props <- maybe (error "Unable to load props") return mprops

    node <- render state props

    (element, evtCallbacks) <- mkReactElement (runHandler args) node

    evtCallbacksRef <- toJSRef evtCallbacks
    js_RenderCbSetResults args evtCallbacksRef element

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
     in elementToM a $ ViewElement (reactView rc) (Nothing :: Maybe (JSRef ())) props childEl

-- | Create an element from a view, and also pass in a key property for the instance.  Key
-- properties speed up the <https://facebook.github.io/react/docs/reconciliation.html reconciliation>
-- of the virtual DOM with the DOM.  The key does not need to be globally unqiue, it only needs to
-- be unique within the siblings of an element.  React allows only strings and numbers as keys.
viewWithKey :: (Typeable props, ToJSRef key)
            => ReactView props -- ^ the view
            -> key -- ^ A value unique within the siblings of this element
            -> props -- ^ The properties to pass to the view instance
            -> ReactElementM eventHandler a -- ^ The children of the view
            -> ReactElementM eventHandler a
viewWithKey rc key props (ReactElementM child) =
    let (a, childEl) = runWriter child
     in elementToM a $ ViewElement (reactView rc) (Just key) props childEl

-- | Create a 'ReactElement' for a class defined in javascript.  For example, if you would like to
-- use <https://github.com/JedWatson/react-select react-select>, you could do so as follows:
--
-- >foreign import javascript unsafe
-- >    "require('react-select')"
-- >    js_GetReactSelectRef :: IO JSRef ()
-- >
-- >reactSelectRef :: JSRef ()
-- >reactSelectRef = unsafePerformIO $ js_GetReactSelectRef
-- >{-# NOINLINE reactSelectRef #-}
-- >
-- >select_ :: [PropertyOrHandler eventHandler] -> ReactElementM eventHandler a
-- >select_ props = foreignClass reactSelectRef props mempty
-- >
-- >onSelectChange :: FromJSON a
-- >               => (a -> handler) -- ^ receives the new value and performs an action.
-- >               -> PropertyOrHandler handler
-- >onSelectChange f = on "onChange" $ \handlerArg -> f $ parse handlerArg
-- >    where
-- >        parse (HandlerArg _ v) =
-- >            case fromJSON v of
-- >                Error err -> error $ "Unable to parse new value for select onChange: " ++ err
-- >                Success e -> e
--
-- This could then be used as part of a rendering function like so:
--
-- >div_ $ select_ [ "name" @= "form-field-name"
-- >               , "value" @= "one"
-- >               , "options" @= [ object [ "value" .= "one", "label" .= "One" ]
-- >                              , object [ "value" .= "two", "label" .= "Two" ]
-- >                              ]
-- >               , onSelectChange $ \newValue -> [AnAction newValue]
-- >               ]
foreignClass :: JSRef cl -- ^ The javascript reference to the class
             -> [PropertyOrHandler eventHandler] -- ^ properties and handlers to pass when creating an instance of this class.
             -> ReactElementM eventHandler a -- ^ The child element or elements
             -> ReactElementM eventHandler a
foreignClass name attrs (ReactElementM child) =
    let (a, childEl) = runWriter child
     in elementToM a $ ForeignElement (Right $ ReactViewRef $ castRef name) attrs childEl
