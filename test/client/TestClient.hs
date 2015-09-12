{-# LANGUAGE OverloadedStrings, TypeFamilies #-}
module Main where

import Control.Monad
import Data.Typeable (Typeable)
import Data.Time
import Data.Maybe
import Debug.Trace
import React.Flux
import React.Flux.Lifecycle
import React.Flux.Internal (toJSString)
import React.Flux.Addons.Intl

import GHCJS.Types (JSRef, JSString)
import GHCJS.Marshal (fromJSRef)

-- TODO: 
-- * Addons
-- * callback property

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

        p_ $
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
        div_ childrenPassedToView

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
--- Intl
--------------------------------------------------------------------------------

intlSpec :: ReactView ()
intlSpec = defineView "intl" $ \() -> setLocales_ "en-US" $
    ul_ $ do
        li_ ["id" $= "f-number"] $
            formattedNumber_ [ "value" @= (0.9 :: Double), "style" $= "percent" ]
        li_ ["id" $= "f-int"] $ int_ 100000
        li_ ["id" $= "f-double"] $ double_ 40000.2

        let moon = fromGregorian 1969 7 20
            fullDayF = DayFormat { weekdayF = Just "long", eraF = Just "short", yearF = Just "2-digit", monthF = Just "long", dayF = Just "2-digit" }

        li_ ["id" $= "f-shortday"] $ day_ shortDate moon
        li_ ["id" $= "f-fullday"] $ day_ fullDayF moon
        li_ ["id" $= "f-date"] $ formattedDate_ (Left moon)
                [ "weekday" $= "short", "month" $= "short", "day" $= "numeric", "year" $= "2-digit" ]

        let step = UTCTime moon (2*60*60 + 56*60) -- 1969-7-20 02:56 UTC
            fullT = (fullDayF, TimeFormat { hourF = Just "numeric", minuteF = Just "2-digit", secondF = Just "numeric", timeZoneNameF = Just "long" })
        
        li_ ["id" $= "f-shorttime"] $ utcTime_ shortDateTime step
        li_ ["id" $= "f-fulltime"] $ utcTime_ fullT step
        li_ ["id" $= "f-time"] $ formattedDate_ (Right step)
                [ "year" $= "2-digit", "month" $= "short", "day" $= "numeric"
                , "hour" $= "numeric", "minute" $= "2-digit", "second" $= "numeric"
                , "timeZoneName" $= "short"
                , "timeZone" $= "Pacific/Tahiti"
                ]

        li_ ["id" $= "f-relative"] $ relativeTo_ step
        li_ ["id" $= "f-relative-days"] $ formattedRelative_ step [ "units" $= "day" ]

        li_ ["id" $= "f-msg"] $
            formattedMessage_
                [ "message" $= "{name} took {numPhotos, plural, =0 {no photos} =1 {one photo} other {# photos}} {takenAgo}."
                , "name" $= "Neil Armstrong"
                , "numPhotos" @= (100 :: Int)
                , elementProperty "takenAgo" $ span_ ["id" $= "takenAgoSpan"] "years ago"
                ]

        li_ ["id" $= "f-html-msg"] $
            formattedHTMLMessage_
                [ "message" $= "<b>{num}</b> is the answer to life, the universe, and everything"
                , "num" @= (42 :: Int)
                ]

--------------------------------------------------------------------------------
--- Main
--------------------------------------------------------------------------------

-- | Test a lifecycle view with all lifecycle methods nothing
app :: ReactView ()
app = defineLifecycleView "app" "Hello" lifecycleConfig
    { lRender = \s () -> do
        eventsView_

        view intlSpec () mempty

        when (s /= "") $
            testLifecycle_ s
        button_ [ "id" $= "add-app-str"
                , onClick $ \_ _ s' -> ([], Just $ s' ++ "o")
                ]
                "Add o"
        button_ [ "id" $= "clear-app-str"
                , onClick $ \_ _ _ -> ([], Just "")
                ] "Clear"
    }

main :: IO ()
main = do
    initializeTouchEvents
    reactRender "app" app ()
