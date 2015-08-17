{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module TestClientSpec (spec) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad
import Data.List
import Test.Hspec.WebDriver
import System.Directory (getCurrentDirectory)
import qualified Data.Text as T

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

spec :: Spec
spec = session " for the test client" $ using Chrome $ do
    it "opens the page" $ runWD $ do
        dir <- liftIO $ getCurrentDirectory
        openPage $ "file://" ++ dir ++ "/../client/test-client.html"
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
        child <- findElem (ById "child-passed-to-view")
        isDisplayed child `shouldReturn` True

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

    {-
    it "inspects the session" $ runWD $ do
        loadLog >>= \x -> liftIO $ putStrLn $ show x
        inspectSession
    -}
