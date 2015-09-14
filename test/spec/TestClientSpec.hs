{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module TestClientSpec (spec) where

import           Control.Monad
import           Control.Monad.IO.Class (liftIO)
import           Data.List
import           Data.Time
import qualified Data.Text              as T
import           System.Directory       (getCurrentDirectory)
import           Test.Hspec.WebDriver

loadLog :: WD [String]
loadLog = executeJS [] "var old = window.test_client_output; window.test_client_output = []; return old;"

shouldBeEvent :: String -> (String, Bool, Int) -> WD ()
shouldBeEvent evt (expectedType, evtBandC, evtPhase) = do
    let prefix = concat
                  [ "Event {evtType = \"" ++ expectedType ++ "\", "
                  , "evtBubbles = " ++ show evtBandC ++ ", "
                  , "evtCancelable = " ++ show evtBandC ++ ", "
                  , "evtCurrentTarget = EventTarget, evtDefaultPrevented = False, "
                  , "evtPhase = " ++ show evtPhase ++ ", "
                  , "evtIsTrusted = False, evtTarget = EventTarget, evtTimestamp = "
                  ]
    unless (prefix `isPrefixOf` evt) $
        error $ "Expecting " ++ prefix ++ " but got " ++ evt
    let suffix = dropWhile (/= ' ') $ drop (length prefix) evt
    suffix `shouldBe` " evtHandlerArg = HandlerArg}"

lifecyclePropsAndStateAre :: String -> Int -> WD ()
lifecyclePropsAndStateAre props st = do
    stE <- findElem (ById "hello")
    getText stE `shouldReturn` (T.pack $ show st)
    p <- findElem (ById "world")
    getText p `shouldReturn` (T.pack $ "Current props: " ++ props)

intlSpanShouldBe :: String -> String -> WD ()
intlSpanShouldBe ident txt = do
    e <- findElem (ById $ T.pack ident)
    getText e `shouldReturn` T.pack txt

-- | Only up to 999,999 since this is just used for the number of days since 1969
showWithComma :: Integer -> String
showWithComma i = show x ++ "," ++ show y
    where
        (x, y) = divMod i 1000

spec :: Spec
spec = do
    describe "React 0.13" $ testClientSpec "test-client.html"
    describe "React 0.14" $ testClientSpec "test-client14.html"

testClientSpec :: String -> Spec
testClientSpec filename = session " for the test client" $ using Chrome $ do
    it "opens the page" $ runWD $ do
        dir <- liftIO $ getCurrentDirectory
        openPage $ "file://" ++ dir ++ "/../client/" ++ filename
        loadLog `shouldReturn`
            [ "will mount"
            , "Current props and state: Hello, 12"
            , "did mount"
            , "Current props and state: Hello, 100"
            , "this id = lifecycle-p"
            , "refStr id = hello"
            , "refProps id = world"
            ]
        lifecyclePropsAndStateAre "Hello" 100

    it "processes a focus event" $ runWD $ do
        findElem (ById "keyinput") >>= click
        [focus, evt] <- loadLog
        focus `shouldBe` "focus"
        evt `shouldBeEvent` ("focus", False, 1)

    it "processes a keydown event" $ runWD $ do
        findElem (ById "keyinput") >>= sendKeys "x"
        [keydown, evt, keyEvt, modState, target, curTarget] <- loadLog
        keydown `shouldBe` "keydown"
        evt `shouldBeEvent` ("keydown", True, 3)
        keyEvt `shouldBe` "(False,0,False,\"Unidentified\",88,\"\",0,False,False,False,88)"
        modState `shouldBe` "alt modifier: False"
        target `shouldBe` "keyinput"
        curTarget `shouldBe` "keyinput"

    it "processes a keydown with alt" $ runWD $ do
        findElem (ById "keyinput") >>= sendKeys "\xE00Ar" -- send Alt-r
        -- generates two events, one for alt, one for r
        [_, _, keyEvt, modState, _, _, _, _, keyEvt2, modState2, _, _] <- loadLog
        keyEvt `shouldBe` "(True,0,False,\"Alt\",18,\"\",0,False,False,False,18)"
        modState `shouldBe` "alt modifier: True"
        keyEvt2 `shouldBe` "(True,0,False,\"Unidentified\",82,\"\",0,False,False,False,82)"
        modState2 `shouldBe` "alt modifier: True"

    it "processes a click event" $ runWD $ do
        findElem (ById "clickinput") >>= click
        [clickName, evt, mouseEvt, modState] <- loadLog
        clickName `shouldBe` "click"
        evt `shouldBeEvent` ("click", True, 3)
        mouseEvt `shouldBe` "(False,0,0,37,54,False,False,37,54,EventTarget,37,54,False)"
        modState `shouldBe` "alt modifier: False"

    {- touch events can't be tested at the moment, chrome doesn't support them
    it "processes a touchinput event" $ runWD $ do
        t <- findElem $ ById "touchinput"
        touchClick t
    -}

    it "stops the default browser action" $ runWD $ do
        findElem (ById "some-link") >>= click
        [x] <- loadLog
        x `shouldBe` "Click some-link"
        url <- getCurrentURL
        unless ("file:" `isPrefixOf` url) $ error "Default browser action was not prevented"

    it "stops propagating an event in the bubbling phase" $ runWD $ do
        findElem (ById "inner-span") >>= moveToCenter
        clickWith LeftButton
        [inner] <- loadLog
        inner `shouldBe` "Click inner span"

    it "stops propagating an event during the capture phase" $ runWD $ do
        findElem (ById "inner-span") >>= moveToCenter
        doubleClick
        [inner, outer] <- loadLog
        inner `shouldBe` "Click inner span"
        outer `shouldBe` "Double click outer div"

    it "has rendered the raw show view" $ runWD $
        (findElem (ById "raw-show-view") >>= getText)
            `shouldReturn` "42"

    describe "lifecycle events" $ do

        it "properly updates the state" $ runWD $ do
            findElem (ById "increment-state") >>= click
            loadLog `shouldReturn`
                [ "will update"
                , "Current props and state: Hello, 100"
                , "New props: Hello"
                , "New state: 101"

                , "did update"
                , "Current props and state: Hello, 101"
                , "Old props: Hello"
                , "Old state: 100"
                ]
            lifecyclePropsAndStateAre "Hello" 101

        it "properly updates the properties" $ runWD $ do
            findElem (ById "add-app-str") >>= click
            loadLog `shouldReturn`
                [ "will recv props"
                , "Current props and state: Hello, 101"
                , "New props: Helloo"

                , "will update"
                , "Current props and state: Hello, 101"
                , "New props: Helloo"
                , "New state: 101"

                , "did update"
                , "Current props and state: Helloo, 101"
                , "Old props: Hello"
                , "Old state: 101"
                ]
            lifecyclePropsAndStateAre "Helloo" 101

        it "unmounts" $ runWD $ do
            findElem (ById "clear-app-str") >>= click
            loadLog `shouldReturn`
                [ "will unmount"
                , "Current props and state: Helloo, 101"
                ]

    describe "intl" $ do
        it "displays the intl formatted data" $ runWD $ do
            "f-number" `intlSpanShouldBe` "90%"
            "f-int" `intlSpanShouldBe` "100,000"
            "f-double" `intlSpanShouldBe` "40,000.2"
            "f-shortday" `intlSpanShouldBe` "Jul 20, 1969"
            "f-fullday" `intlSpanShouldBe` "Sunday, July 20, 69 AD"
            "f-date" `intlSpanShouldBe` "Sun, Jul 20, 69"
            -- f-shorttime and f-fulltime cannot be (easily) tested since they rely on the current timezone
            "f-time" `intlSpanShouldBe` "Jul 19, 69, 4:56:00 PM GMT-10"

            today <- liftIO (utctDay <$> getCurrentTime)
            let moon = fromGregorian 1969 7 20
                daysAgo = diffDays today moon
                yearsAgo :: Int = round $ realToFrac daysAgo / (365 :: Double) -- is close enough
            "f-relative" `intlSpanShouldBe` (show yearsAgo ++ " years ago")
            "f-relative-days" `intlSpanShouldBe` (showWithComma daysAgo ++ " days ago")

        it "displays messages" $ runWD $ do
            msg <- findElem $ ById "f-msg"
            getText msg `shouldReturn` "Neil Armstrong took 100 photos years ago."
            takenAgoSpan <- findElemFrom msg $ ById "takenAgoSpan"
            getText takenAgoSpan `shouldReturn` "years ago"

            htmlMsg <- findElem $ ById "f-html-msg"
            getText htmlMsg `shouldReturn` "42 is the answer to life, the universe, and everything"
            b <- findElemFrom htmlMsg $ ByTag "b"
            getText b `shouldReturn` "42"

    describe "children passed to view" $ do

        it "does not display null children" $ runWD $ do
            s <- findElem $ ById "empty-children"
            findElemsFrom s (ByCSS "*") `shouldReturn` []
            getText s `shouldReturn` ""

        it "displays a single child" $ runWD $ do
            s <- findElem $ ById "single-child-wrapper"
            s' <- findElemFrom s $ ByCSS "span#single-child"
            getText s' `shouldReturn` "Single Child!!"

        it "displays a child list" $ runWD $ do
            s <- findElem $ ById "multi-child"
            c1 <- findElemFrom s $ ByCSS "span#child1"
            getText c1 `shouldReturn` "Child 1"
            c2 <- findElemFrom s $ ByCSS "span#child2"
            getText c2 `shouldReturn` "Child 2"

    it "displays the elements inside the transition" $ runWD $ do
        d <- findElem $ ById "css-transitions"
        [a, b] <- findElemsFrom d $ ByCSS "span.css-transition-entry"
        getText a `shouldReturn` "A"
        getText b `shouldReturn` "B"

    describe "bootstrap" $ do

        it "displays and closes alert" $ runWD $ do
            alert <- findElem $ ByCSS "div#bootstrap div.alert"
            (findElemFrom alert (ByTag "p") >>= getText)
                `shouldReturn` "Hello, World!"
            findElemFrom alert (ByTag "button") >>= click
            loadLog `shouldReturn` ["Closing alert"]

        it "switchs nav items" $ runWD $ do
            navUl <- findElem $ ByCSS "div#bootstrap nav > ul"
            i1 <- findElemFrom navUl $ ByCSS "li:first-child a"
            i2 <- findElemFrom navUl $ ByCSS "li:nth-child(2) a"
            i3 <- findElemFrom navUl $ ByCSS "li:nth-child(3) a"
            click i2
            loadLog `shouldReturn` ["Switched to 2"]
            click i3
            loadLog `shouldReturn` ["Switched to 3"]
            click i1
            loadLog `shouldReturn` ["Switched to 1"]

    {-
    it "inspects the session" $ runWD $ do
        loadLog >>= \x -> liftIO $ putStrLn $ show x
        inspectSession
    -}
