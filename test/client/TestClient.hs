{-# LANGUAGE OverloadedStrings, TypeFamilies, ScopedTypeVariables, DeriveAnyClass, FlexibleInstances, DeriveGeneric #-}
module TestClient (testClient) where

import Control.DeepSeq (NFData)
import Control.Monad
import Data.Monoid ((<>))
import Data.Typeable (Typeable)
import Data.Maybe
import Debug.Trace
import GHC.Generics (Generic)
import React.Flux
import React.Flux.Lifecycle
import React.Flux.Internal (toJSString)
import React.Flux.Addons.React
import React.Flux.Addons.Bootstrap

import GHCJS.Types (JSRef, JSString)
import GHCJS.Marshal (fromJSRef)

foreign import javascript unsafe
    "(function(x) { \
    \    if (!window.test_client_output) window.test_client_output = []; \
    \    window.test_client_output.push(x); \
    \})($1)"
    js_output :: JSString -> IO ()

data OutputStoreData = OutputStoreData
    deriving (Show, Typeable)

instance StoreData OutputStoreData where
    type StoreAction OutputStoreData = [String]
    -- log both to the console and to js_output
    transform ss OutputStoreData = do
        mapM_ (js_output . toJSString) ss
        trace (unlines ss) $ return OutputStoreData

outputStore :: ReactStore OutputStoreData
outputStore = mkStore OutputStoreData

output :: [String] -> [SomeStoreAction]
output s = [SomeStoreAction outputStore s]

outputIO :: [String] -> IO ()
outputIO ss = void $ transform ss OutputStoreData

--------------------------------------------------------------------------------
--- Events
--------------------------------------------------------------------------------

logM :: (String -> Bool) -> String
logM f = "alt modifier: " ++ show (f "Alt")

logT :: EventTarget -> String
logT t = eventTargetProp t "id"

rawShowView :: ReactView Int
rawShowView = defineView "raw show view" elemShow

eventsView :: ReactView ()
eventsView = defineView "events" $ \() ->
    div_ $ do
        p_ $ input_ [ "type" $= "text"
                    , "id" $= "keyinput"
                    , "placeholder" $= "onKeyDown"
                    , onKeyDown $ \e k -> output
                        [ "keydown"
                        , show e
                        , show k
                        , logM (keyGetModifierState k)
                        , logT (evtTarget e)
                        , logT (evtCurrentTarget e)
                        ]
                    , onFocus $ \e _ -> output
                        [ "focus"
                        , show e
                        --, logT $ focusRelatedTarget f
                        ]
                    ]

        p_ $ label_ [ "id" $= "clickinput"
                    , onClick $ \e m -> output
                        [ "click"
                        , show e
                        , show m 
                        , logM (mouseGetModifierState m)
                        --, logT (mouseRelatedTarget m)
                        ]
                    ]
           "onClick"

        p_ $ label_ [ "id" $= "touchinput"
                    , onTouchStart $ \e t -> output
                        [ "touchstart"
                        , show e
                        , show t
                        , logM (touchGetModifierState t)
                        , logT (touchTarget $ head $ touchTargets t)
                        , "endtouch"
                        ]
                    ]
           "onTouchStart"

        p_ $ a_ [ "id" $= "some-link"
                , "href" $= "http://www.haskell.org"
                , onClick $ \e _ -> output ["Click some-link"] ++ [preventDefault e]
                ]
                "Testing preventDefault"

        div_ $
            div_ [ "id" $= "outer-div"
                 , onClick $ \_ _ -> output ["Click on outer div"]
                 , capturePhase $ onDoubleClick $ \e _ -> output ["Double click outer div"] ++ [stopPropagation e]
                 ] $ do
                
                span_ [ "id" $= "inner-span"
                      , onClick $ \e _ -> output ["Click inner span"] ++ [stopPropagation e]
                      , onDoubleClick $ \_ _ -> output ["Double click inner span"]
                      ]
                      "Testing stopPropagation"

        p_ [ "id" $= "raw-show-view"] $ view rawShowView 42 mempty

eventsView_ :: ReactElementM eventHandler ()
eventsView_ = view eventsView () mempty

--------------------------------------------------------------------------------
--- Lifecycle
--------------------------------------------------------------------------------

logPandS :: LPropsAndState String Int -> IO ()
logPandS ps = do
    p <- lGetProps ps
    st <- lGetState ps
    outputIO ["Current props and state: " ++ p ++ ", " ++ show st]

foreign import javascript unsafe
    "$1.id"
    js_domGetId :: JSRef -> IO JSRef

logDOM :: LDOM -> IO ()
logDOM dom = do
    this <- lThis dom >>= js_domGetId >>= fromJSRef
    x <- lRef dom "refSt" >>= js_domGetId >>= fromJSRef
    y <- lRef dom "refProps" >>= js_domGetId >>= fromJSRef
    outputIO [ "this id = " ++ fromMaybe "Nothing" this
             , "refStr id = " ++ fromMaybe "Nothing" x
             , "refProps id = " ++ fromMaybe "Nothing" y
             ]


testLifecycle :: ReactView String
testLifecycle = defineLifecycleView "testlifecycle" (12 :: Int) lifecycleConfig
    { lRender = \s p -> p_ ["id" $= "lifecycle-p"] $ do
        span_ "Current state: "
        span_ ["ref" $= "refSt", "id" $= "hello"] (elemShow s)
        span_ ["ref" $= "refProps", "id" $= "world"] $ elemText $ "Current props: " ++ p
        button_ [ "id" $= "increment-state"
                , onClick $ \_ _ st -> ([], Just $ st + 1)
                ] "Incr"

    , lComponentWillMount = Just $ \pAndS setStateFn -> do
        outputIO ["will mount"]
        logPandS pAndS
        setStateFn 100

    , lComponentDidMount = Just $ \pAndS dom _setStateFn -> do
        outputIO ["did mount"]
        logPandS pAndS
        logDOM dom

    , lComponentWillReceiveProps = Just $ \pAndS _dom _setStateFn newProps -> do
        outputIO ["will recv props"]
        logPandS pAndS
        outputIO ["New props: " ++ newProps]

    , lComponentWillUpdate = Just $ \pAndS _dom newProps newState -> do
        outputIO ["will update"]
        logPandS pAndS
        outputIO ["New props: " ++ newProps, "New state: " ++ show newState]

    , lComponentDidUpdate = Just $ \pAndS _dom _setStateFn oldProps oldState -> do
        outputIO ["did update"]
        logPandS pAndS
        outputIO ["Old props: " ++ oldProps, "Old state: " ++ show oldState]

    , lComponentWillUnmount = Just $ \pAndS _dom -> do
        outputIO ["will unmount"]
        logPandS pAndS
    }

testLifecycle_ :: String -> ReactElementM eventHandler ()
testLifecycle_ s = view testLifecycle s $ span_ ["id" $= "child-passed-to-view"] "I am a child!!!"

--------------------------------------------------------------------------------
--- Children passed to view
--------------------------------------------------------------------------------

displayChildren :: ReactView String
displayChildren = defineView "display children" $ \ident ->
    span_ [classNames [("display-children", True), ("missing-name", False)], "id" @= ident]
        childrenPassedToView

displayChildren_ :: String -> ReactElementM handler () -> ReactElementM handler ()
displayChildren_ = view displayChildren

displayChildrenSpec :: ReactElementM handler ()
displayChildrenSpec = ul_ $ do
    li_ $ displayChildren_ "empty-children" mempty
    li_ $ displayChildren_ "single-child-wrapper" $ span_ ["id" $= "single-child"] "Single Child!!"
    li_ $ displayChildren_ "multi-child" $ span_ ["id" $= "child1"] "Child 1" <> span_ ["id" $= "child2"] "Child 2"

--------------------------------------------------------------------------------
--- CSS Transitions
--------------------------------------------------------------------------------

cssTransitions :: ReactView [String]
cssTransitions = defineView "css transitions" $ \items ->
    div_ ["id" $= "css-transitions"] $
        cssTransitionGroup ["transitionName" $= "example"] $
            forM_ (zip items [(0 :: Int)..]) $ \(txt, key) ->
                div_ ["key" @= key] $ span_ ["className" $= "css-transition-entry"] $ elemText txt

--------------------------------------------------------------------------------
--- Bootstrap
--------------------------------------------------------------------------------

bootstrapSpec :: ReactView ()
bootstrapSpec = defineView "bootstrap" $ \() -> div_ ["id" $= "bootstrap"] $ do
    bootstrap_ "Alert" [ "bsStyle" $= "danger"
                       , callback "onDismiss" $ output ["Closing alert"]
                       ] $
        p_ "Hello, World!"

    bootstrap_ "Nav" [ "activeKey" @= (1 :: Int)
                     , callback "onSelect" $ \(i :: Int) -> output ["Switched to " ++ show i]
                     ] $ do
        bootstrap_ "NavItem" ["eventKey" @= (1 :: Int)] "Item 1"
        bootstrap_ "NavItem" ["eventKey" @= (2 :: Int)] "Item 2"
        bootstrap_ "NavItem" ["eventKey" @= (3 :: Int)] "Item 3"

--------------------------------------------------------------------------------
--- shouldComponentUpdate
--------------------------------------------------------------------------------

data ShouldComponentUpdateData = ShouldComponentUpdateData Int String
    deriving (Typeable, Show)

data ShouldComponentUpdateAction = IncrementAllSCUData
                                 | IncrementFirstSCUData
                                 | NoChangeToSCUData
    deriving (Show, Typeable, Generic, NFData)

instance StoreData [ShouldComponentUpdateData] where
    type StoreAction [ShouldComponentUpdateData] = ShouldComponentUpdateAction
    transform NoChangeToSCUData s = return s
    transform IncrementAllSCUData d = return [ShouldComponentUpdateData (i+1) s | ShouldComponentUpdateData i s <- d]
    transform IncrementFirstSCUData (ShouldComponentUpdateData i s:xs) = return $ (ShouldComponentUpdateData (i+1) s) : xs
    transform _ [] = error "Should never happen"

shouldComponentUpdateStore :: ReactStore [ShouldComponentUpdateData]
shouldComponentUpdateStore = mkStore [ ShouldComponentUpdateData 1 "Hello"
                                     , ShouldComponentUpdateData 2 "World"
                                     , ShouldComponentUpdateData 3 "!!!"
                                     ]

-- | This will log wheenver componentWillUpdate lifecycle event occurs
logComponentWillUpdate :: ReactView ShouldComponentUpdateData
logComponentWillUpdate = defineLifecycleView "shouldComponentUpdate spec" () lifecycleConfig
    { lRender = \() (ShouldComponentUpdateData i s) ->
                    span_ (elemShow i) <> span_ (elemText s)
    , lComponentWillUpdate = Just $ \curProps _ (ShouldComponentUpdateData newI newS) () -> do
          ShouldComponentUpdateData curI curS <- lGetProps curProps
          outputIO [ "Component will update"
                   , "current props: " ++ show curI ++ " " ++ curS
                   , "new props: " ++ show newI ++ " " ++ newS
                   ]
    }

shouldComponentUpdateSpec :: ReactView ()
shouldComponentUpdateSpec = defineControllerView "should component update" shouldComponentUpdateStore $ \ds () -> 
    div_ ["id" $= "should-component-update"] $ do
        ul_ $ forM_ (zip ds [(0 :: Int)..]) $ \(su, i)  ->
            li_ $ viewWithKey logComponentWillUpdate i su mempty

        button_ ["id" $= "no-change-scu", onClick $ \_ _ -> [SomeStoreAction shouldComponentUpdateStore NoChangeToSCUData]]
            "No change to data"

        button_ ["id" $= "change-all-scu", onClick $ \_ _ -> [SomeStoreAction shouldComponentUpdateStore IncrementAllSCUData]]
            "Increment all"

        button_ ["id" $= "increment-first-scu", onClick $ \_ _ -> [SomeStoreAction shouldComponentUpdateStore IncrementFirstSCUData]]
            "Increment first entry's integer"

--------------------------------------------------------------------------------
--- Main
--------------------------------------------------------------------------------

-- | Test a lifecycle view with all lifecycle methods nothing
testClient :: ReactView ()
testClient = defineLifecycleView "app" "Hello" lifecycleConfig
    { lRender = \s () -> do
        eventsView_

        when (s /= "") $
            testLifecycle_ s
        button_ [ "id" $= "add-app-str"
                , onClick $ \_ _ s' -> ([], Just $ s' ++ "o")
                ]
                "Add o"
        button_ [ "id" $= "clear-app-str"
                , onClick $ \_ _ _ -> ([], Just "")
                ] "Clear"

        displayChildrenSpec

        view cssTransitions ["A", "B"] mempty

        view bootstrapSpec () mempty

        view shouldComponentUpdateSpec () mempty
    }
