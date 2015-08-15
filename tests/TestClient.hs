{-# LANGUAGE OverloadedStrings #-}
module Main where

import React.Flux
import React.Flux.Lifecycle
import Debug.Trace
import Control.Monad

import GHCJS.Types (JSRef)

-- TODO: 
-- * stopPropagation, preventDefault
-- * Addons
-- * lifecycle with and without specified callbacks
-- * callback property

logM :: (String -> Bool) -> String
logM f = "modifier: " ++ show (f "")

logT :: EventTarget -> String
logT t = eventTargetProp t "id"

consoleLog :: [String] -> [SomeStoreAction]
consoleLog s = trace (unlines s) []

app :: ReactView ()
app = defineView "app" $ \() ->
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

        div_ [ onWheel $ \e w -> trace (show e ++ " ### wheel ### " ++ show w) []
             , onScroll $ \e s -> trace (show e ++ " ### scroll ### " ++ show s) []
             ] $ p_ "Hello, World"

        forM_ [0..(200 :: Int)] $ \i ->
            p_ $ elemShow i




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
    { lRender = \s p -> do
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
testLifecycle_ s = view testLifecycle s $ span_ "I am a child!!!"

main :: IO ()
main = do
    initializeTouchEvents
    reactRender "app" app ()
