{-# LANGUAGE OverloadedStrings #-}
module Main where

import React.Flux
import Debug.Trace
import Control.Monad

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

main :: IO ()
main = do
    initializeTouchEvents
    reactRender "app" app ()
