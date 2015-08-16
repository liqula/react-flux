{-# LANGUAGE OverloadedStrings #-}
module Main where

import React.Flux
import React.Flux.Lifecycle
import Debug.Trace

import GHCJS.Types (JSRef)

-- TODO: 
-- * Addons
-- * callback property

--------------------------------------------------------------------------------
--- Events
--------------------------------------------------------------------------------

logM :: (String -> Bool) -> String
logM f = "alt modifier: " ++ show (f "Alt")

logT :: EventTarget -> String
logT t = eventTargetProp t "id"

consoleLog :: [String] -> [SomeStoreAction]
consoleLog s = trace (unlines s) []

eventsView :: ReactView ()
eventsView = defineView "events" $ \() ->
    div_ $ do
        p_ $ input_ [ "type" $= "text"
                    , "id" $= "keyinput"
                    , "placeholder" $= "onKeyDown"
                    , onKeyDown $ \e k -> consoleLog
                        [ "keydown"
                        , show e
                        , show k
                        , logM (keyGetModifierState k)
                        , logT (evtTarget e)
                        , logT (evtCurrentTarget e)
                        ]
                    , onFocus $ \e f -> consoleLog
                        [ "focus"
                        , show e
                        , logT $ focusRelatedTarget f
                        ]
                    ]

        p_ $ label_ [ "id" $= "clickinput"
                    , onClick $ \e m -> consoleLog
                        [ "click"
                        , show e
                        , show m 
                        , logM (mouseGetModifierState m)
                        --, logT (mouseRelatedTarget m)
                        ]
                    ]
           "onClick"

        p_ $ label_ [ "id" $= "touchinput"
                    , onTouchStart $ \e t -> consoleLog
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
                , onClick $ \e _ -> trace "Click some-link" [preventDefault e]
                ]
                "Testing preventDefault"

        p_ $
            div_ [ "id" $= "outer-div"
                 , onClick $ \_ _ -> trace "Click on outer div" []
                 , capturePhase $ onDoubleClick $ \e _ -> trace "Double click outer div" [stopPropagation e]
                 ] $ do
                
                span_ [ "id" $= "inner-span"
                      , onClick $ \e _ -> trace "Click inner span" [stopPropagation e]
                      , onDoubleClick $ \_ _ -> trace "Double click inner span" []
                      ]
                      "Testing stopPropagation"

eventsView_ :: ReactElementM eventHandler ()
eventsView_ = view eventsView () mempty

--------------------------------------------------------------------------------
--- Lifecycle
--------------------------------------------------------------------------------

logPandS :: LPropsAndState String Int -> IO ()
logPandS ps = do
    p <- lGetProps ps
    st <- lGetState ps
    trace ("Current props and state: " ++ p ++ ", " ++ show st) $ return ()

foreign import javascript
    "console.log($1)"
    js_logElem :: JSRef a -> IO ()

logDOM :: LDOM -> IO ()
logDOM dom = do
    lThis dom >>= js_logElem
    lRef dom "refSt" >>= js_logElem
    lRef dom "refProps" >>= js_logElem

testLifecycle :: ReactView String
testLifecycle = defineLifecycleView "testlifecycle" (12 :: Int) lifecycleConfig
    { lRender = \s p -> p_ $ do
        span_ "Current state: "
        span_ ["ref" $= "refSt", "id" $= "hello"] (elemShow s)
        span_ ["ref" $= "refProps", "id" $= "world"] $ elemText $ "Current props: " ++ p
        button_ [ onClick $ \_ _ st -> ([], Just $ st + 1) ] "Incr"
        div_ childrenPassedToView

    , lComponentWillMount = Just $ \pAndS setStateFn -> trace "will mount" $ do
        logPandS pAndS
        setStateFn 100

    , lComponentDidMount = Just $ \pAndS dom _setStateFn -> trace "did mount" $ do
        logPandS pAndS
        logDOM dom

    , lComponentWillReceiveProps = Just $ \pAndS dom _setStateFn newProps -> trace "will recv props" $ do
        logPandS pAndS
        logDOM dom
        trace ("New props: " ++ newProps) $ return ()

    , lComponentWillUpdate = Just $ \pAndS dom newProps newState -> trace "will update" $ do
        logPandS pAndS
        logDOM dom
        trace ("New props: " ++ newProps) $ trace ("New state: " ++ show newState) $ return ()

    , lComponentDidUpdate = Just $ \pAndS dom _setStateFn oldProps oldState -> trace "did update" $ do
        logPandS pAndS
        logDOM dom
        trace ("Old props: " ++ oldProps) $ trace ("Old state: " ++ show oldState) $ return ()

    , lComponentWillUnmount = Just $ \pAndS dom -> trace "will unmount" $ do
        logPandS pAndS
        logDOM dom
    }

testLifecycle_ :: String -> ReactElementM eventHandler ()
testLifecycle_ s = view testLifecycle s $ span_ ["id" $= "child-passed-to-view"] "I am a child!!!"

--------------------------------------------------------------------------------
--- Main
--------------------------------------------------------------------------------

-- | Test a lifecycle view with all lifecycle methods nothing
app :: ReactView ()
app = defineLifecycleView "app" "Hello" lifecycleConfig
    { lRender = \s () -> do
        eventsView_
        testLifecycle_ s
        button_ [ "id" $= "add-app-str"
                , onClick $ \_ _ s' -> ([], Just $ s' ++ "o")
                ]
                "Add o"
    }

main :: IO ()
main = do
    initializeTouchEvents
    reactRender "app" app ()
