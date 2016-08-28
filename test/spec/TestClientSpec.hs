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
                  , "evtIsTrusted = True, evtTarget = EventTarget, evtTimestamp = "
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

scuShouldBe :: String -> [String] -> WD ()
scuShouldBe ident items = do
    d <- findElem $ ById $ T.pack ident
    lis <- findElemsFrom d $ ByTag "li"
    length lis `shouldBe` length items
    forM_ (zip lis items) $ \(li,i) ->
        getText li `shouldReturn` T.pack i

scuSingleShouldBe :: [String] -> WD ()
scuSingleShouldBe = scuShouldBe "should-component-update-single"

scuPairShouldBe :: [String] -> WD ()
scuPairShouldBe = scuShouldBe "should-component-update-pair"

scuTripleShouldBe :: [String] -> WD ()
scuTripleShouldBe = scuShouldBe "should-component-update-triple"

scuSingleLogMsg :: String -> String -> [String]
scuSingleLogMsg current new = ["Component will update single", "current props: " ++ current, "new props: " ++ new]

scuPairLogMsg :: String -> String -> [String]
scuPairLogMsg current new = ["Component will update for pair input view", "current props: " ++ current, "new props: " ++ new]

scuTripleLogMsg :: String -> String -> [String]
scuTripleLogMsg current new = ["Component will update for triple input view", "current props: " ++ current, "new props: " ++ new]

intlSpanShouldBe :: String -> String -> WD ()
intlSpanShouldBe ident txt = do
    e <- findElem (ById $ T.pack ident)
    getText e `shouldReturn` T.pack txt

intlPlaceholderShouldBe :: String -> String -> WD ()
intlPlaceholderShouldBe ident txt = do
    e <- findElem (ById $ T.pack ident)
    input <- findElemFrom e $ ByTag "input"
    (input `attr` "placeholder") `shouldReturn` Just (T.pack txt)

-- | Only up to 999,999 since this is just used for the number of days since 1969
showWithComma :: Integer -> String
showWithComma i = show x ++ "," ++ replicate (3-length y') '0' ++ y'
    where
        (x, y) = divMod i 1000
        y' = show y

spec :: Spec
spec = testClientSpec "test-client.html"

allBrowsers :: [Capabilities]
allBrowsers = [chromeCaps]

testClientSpec :: String -> Spec
testClientSpec filename = session " for the test client" $ using allBrowsers $ do
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
        keyEvt `shouldBe` "(False,0,False,\"Unidentified\",88,Nothing,0,False,False,False,88)"
        modState `shouldBe` "alt modifier: False"
        target `shouldBe` "keyinput"
        curTarget `shouldBe` "keyinput"

    it "processes a keydown with alt" $ runWD $ do
        findElem (ById "keyinput") >>= sendKeys "\xE00Ar" -- send Alt-r
        -- generates two events, one for alt, one for r
        [_, _, keyEvt, modState, _, _, _, _, keyEvt2, modState2, _, _] <- loadLog
        keyEvt `shouldBe` "(True,0,False,\"Alt\",18,Nothing,0,False,False,False,18)"
        modState `shouldBe` "alt modifier: True"
        keyEvt2 `shouldBe` "(True,0,False,\"Unidentified\",82,Nothing,0,False,False,False,82)"
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

    describe "children passed to view" $ do

        it "does not display null children" $ runWD $ do
            s <- findElem $ ById "empty-children"
            s `attr` "class" `shouldReturn` Just "display-children"
            findElemsFrom s (ByCSS "*") `shouldReturn` []
            getText s `shouldReturn` ""

        it "displays a single child" $ runWD $ do
            s <- findElem $ ById "single-child-wrapper"
            s `attr` "class" `shouldReturn` Just "display-children"
            s' <- findElemFrom s $ ByCSS "span#single-child"
            getText s' `shouldReturn` "Single Child!!"

        it "displays a child list" $ runWD $ do
            s <- findElem $ ById "multi-child"
            s `attr` "class" `shouldReturn` Just "display-children"
            c1 <- findElemFrom s $ ByCSS "span#child1"
            getText c1 `shouldReturn` "Child 1"
            c2 <- findElemFrom s $ ByCSS "span#child2"
            getText c2 `shouldReturn` "Child 2"

    it "displays the elements inside the transition" $ runWD $ do
        d <- findElem $ ById "css-transitions"
        [a, b] <- findElemsFrom d $ ByCSS "span.css-transition-entry"
        getText a `shouldReturn` "A"
        getText b `shouldReturn` "B"

    it "renders a callback returning a view" $ runWD $ do
        e <- findElem $ ById "callback-view-props-test"
        getText e `shouldReturn` "Props are 5 and Hello World"

    describe "should component update" $ do

        it "has the initial data" $ runWD $ do
            scuSingleShouldBe ["1Quick Ben", "5Karsa", "9Anomander Rake"]
            scuPairShouldBe ["1Quick Ben2Whiskeyjack", "5Karsa6Tehol", "9Anomander Rake10Iskaral Pust"]
            scuTripleShouldBe ["1Quick Ben2Whiskeyjack3Fiddler", "5Karsa6Tehol7Tayschrenn", "9Anomander Rake10Iskaral Pust11Dujek"]

        it "does not update when no change to data" $ runWD $ do
            findElem (ById "no-change-scu") >>= click
            scuSingleShouldBe ["1Quick Ben", "5Karsa", "9Anomander Rake"]
            scuPairShouldBe ["1Quick Ben2Whiskeyjack", "5Karsa6Tehol", "9Anomander Rake10Iskaral Pust"]
            scuTripleShouldBe ["1Quick Ben2Whiskeyjack3Fiddler", "5Karsa6Tehol7Tayschrenn", "9Anomander Rake10Iskaral Pust11Dujek"]
            loadLog `shouldReturn` []

        describe "Changing the first scu" $ do

            it "increments just the head of the list without re-rendering all entries" $ runWD $ do
                findElem (ById "increment-first-scuSCU1") >>= click
                scuSingleShouldBe ["2Quick Ben", "5Karsa", "9Anomander Rake"]
                scuPairShouldBe ["2Quick Ben2Whiskeyjack", "5Karsa6Tehol", "9Anomander Rake10Iskaral Pust"]
                scuTripleShouldBe ["2Quick Ben2Whiskeyjack3Fiddler", "5Karsa6Tehol7Tayschrenn", "9Anomander Rake10Iskaral Pust11Dujek"]
                loadLog `shouldReturn` concat
                    [ scuSingleLogMsg "1 Quick Ben"
                                      "2 Quick Ben"
                    , scuPairLogMsg "1 Quick Ben 2 Whiskeyjack"
                                    "2 Quick Ben 2 Whiskeyjack"
                    , scuTripleLogMsg "1 Quick Ben 2 Whiskeyjack 3 Fiddler"
                                      "2 Quick Ben 2 Whiskeyjack 3 Fiddler"
                    ]


            it "increments all entries" $ runWD $ do
                findElem (ById "change-all-scu-SCU1") >>= click
                scuSingleShouldBe ["3Quick Ben", "6Karsa", "10Anomander Rake"]
                scuPairShouldBe ["3Quick Ben2Whiskeyjack", "6Karsa6Tehol", "10Anomander Rake10Iskaral Pust"]
                scuTripleShouldBe ["3Quick Ben2Whiskeyjack3Fiddler", "6Karsa6Tehol7Tayschrenn", "10Anomander Rake10Iskaral Pust11Dujek"]
                loadLog `shouldReturn` concat
                    [ scuSingleLogMsg "2 Quick Ben"
                                      "3 Quick Ben"
                    , scuSingleLogMsg "5 Karsa"
                                      "6 Karsa"
                    , scuSingleLogMsg "9 Anomander Rake"
                                      "10 Anomander Rake"
                    , scuPairLogMsg   "2 Quick Ben 2 Whiskeyjack"
                                      "3 Quick Ben 2 Whiskeyjack"
                    , scuPairLogMsg   "5 Karsa 6 Tehol"
                                      "6 Karsa 6 Tehol"
                    , scuPairLogMsg   "9 Anomander Rake 10 Iskaral Pust"
                                      "10 Anomander Rake 10 Iskaral Pust"
                    , scuTripleLogMsg "2 Quick Ben 2 Whiskeyjack 3 Fiddler"
                                      "3 Quick Ben 2 Whiskeyjack 3 Fiddler"
                    , scuTripleLogMsg "5 Karsa 6 Tehol 7 Tayschrenn"
                                      "6 Karsa 6 Tehol 7 Tayschrenn"
                    , scuTripleLogMsg "9 Anomander Rake 10 Iskaral Pust 11 Dujek"
                                      "10 Anomander Rake 10 Iskaral Pust 11 Dujek"
                    ]

        describe "Changing the second scu" $ do

            it "increments just the head of the list without re-rendering all entries" $ runWD $ do
                findElem (ById "increment-first-scuSCU2") >>= click
                scuSingleShouldBe ["3Quick Ben", "6Karsa", "10Anomander Rake"] -- unchanged
                scuPairShouldBe ["3Quick Ben3Whiskeyjack", "6Karsa6Tehol", "10Anomander Rake10Iskaral Pust"]
                scuTripleShouldBe ["3Quick Ben3Whiskeyjack3Fiddler", "6Karsa6Tehol7Tayschrenn", "10Anomander Rake10Iskaral Pust11Dujek"]
                loadLog `shouldReturn` concat
                    [ scuPairLogMsg "3 Quick Ben 2 Whiskeyjack"
                                    "3 Quick Ben 3 Whiskeyjack"
                    , scuTripleLogMsg "3 Quick Ben 2 Whiskeyjack 3 Fiddler"
                                      "3 Quick Ben 3 Whiskeyjack 3 Fiddler"
                    ]


            it "increments all entries" $ runWD $ do
                findElem (ById "change-all-scu-SCU2") >>= click
                scuSingleShouldBe ["3Quick Ben", "6Karsa", "10Anomander Rake"] -- unchanged
                scuPairShouldBe ["3Quick Ben4Whiskeyjack", "6Karsa7Tehol", "10Anomander Rake11Iskaral Pust"]
                scuTripleShouldBe ["3Quick Ben4Whiskeyjack3Fiddler", "6Karsa7Tehol7Tayschrenn", "10Anomander Rake11Iskaral Pust11Dujek"]
                loadLog `shouldReturn` concat
                    [ scuPairLogMsg   "3 Quick Ben 3 Whiskeyjack"
                                      "3 Quick Ben 4 Whiskeyjack"
                    , scuPairLogMsg   "6 Karsa 6 Tehol"
                                      "6 Karsa 7 Tehol"
                    , scuPairLogMsg   "10 Anomander Rake 10 Iskaral Pust"
                                      "10 Anomander Rake 11 Iskaral Pust"
                    , scuTripleLogMsg "3 Quick Ben 3 Whiskeyjack 3 Fiddler"
                                      "3 Quick Ben 4 Whiskeyjack 3 Fiddler"
                    , scuTripleLogMsg "6 Karsa 6 Tehol 7 Tayschrenn"
                                      "6 Karsa 7 Tehol 7 Tayschrenn"
                    , scuTripleLogMsg "10 Anomander Rake 10 Iskaral Pust 11 Dujek"
                                      "10 Anomander Rake 11 Iskaral Pust 11 Dujek"
                    ]

        describe "Changing the third scu" $ do

            it "increments just the head of the list without re-rendering all entries" $ runWD $ do
                findElem (ById "increment-first-scuSCU3") >>= click
                scuSingleShouldBe ["3Quick Ben", "6Karsa", "10Anomander Rake"] -- unchanged
                scuPairShouldBe ["3Quick Ben4Whiskeyjack", "6Karsa7Tehol", "10Anomander Rake11Iskaral Pust"] -- unchanged
                scuTripleShouldBe ["3Quick Ben4Whiskeyjack4Fiddler", "6Karsa7Tehol7Tayschrenn", "10Anomander Rake11Iskaral Pust11Dujek"]
                loadLog `shouldReturn` concat
                    [ scuTripleLogMsg "3 Quick Ben 4 Whiskeyjack 3 Fiddler"
                                      "3 Quick Ben 4 Whiskeyjack 4 Fiddler"
                    ]


            it "increments all entries" $ runWD $ do
                findElem (ById "change-all-scu-SCU3") >>= click
                scuSingleShouldBe ["3Quick Ben", "6Karsa", "10Anomander Rake"] -- unchanged
                scuPairShouldBe ["3Quick Ben4Whiskeyjack", "6Karsa7Tehol", "10Anomander Rake11Iskaral Pust"] -- unchanged
                scuTripleShouldBe ["3Quick Ben4Whiskeyjack5Fiddler", "6Karsa7Tehol8Tayschrenn", "10Anomander Rake11Iskaral Pust12Dujek"]
                loadLog `shouldReturn` concat
                    [ scuTripleLogMsg "3 Quick Ben 4 Whiskeyjack 4 Fiddler"
                                      "3 Quick Ben 4 Whiskeyjack 5 Fiddler"
                    , scuTripleLogMsg "6 Karsa 7 Tehol 7 Tayschrenn"
                                      "6 Karsa 7 Tehol 8 Tayschrenn"
                    , scuTripleLogMsg "10 Anomander Rake 11 Iskaral Pust 11 Dujek"
                                      "10 Anomander Rake 11 Iskaral Pust 12 Dujek"
                    ]

        describe "changing the fourth scu" $ do

            it "increments just the head of the list" $ runWD $ do
                findElem (ById "increment-first-scuSCU4") >>= click
                -- unchanged
                scuSingleShouldBe ["3Quick Ben", "6Karsa", "10Anomander Rake"] -- unchanged
                scuPairShouldBe ["3Quick Ben4Whiskeyjack", "6Karsa7Tehol", "10Anomander Rake11Iskaral Pust"] -- unchanged
                scuTripleShouldBe ["3Quick Ben4Whiskeyjack5Fiddler", "6Karsa7Tehol8Tayschrenn", "10Anomander Rake11Iskaral Pust12Dujek"]
                loadLog `shouldReturn` []

            it "increments all entries" $ runWD $ do
                findElem (ById "change-all-scu-SCU4") >>= click
                -- unchanged
                scuSingleShouldBe ["3Quick Ben", "6Karsa", "10Anomander Rake"] -- unchanged
                scuPairShouldBe ["3Quick Ben4Whiskeyjack", "6Karsa7Tehol", "10Anomander Rake11Iskaral Pust"] -- unchanged
                scuTripleShouldBe ["3Quick Ben4Whiskeyjack5Fiddler", "6Karsa7Tehol8Tayschrenn", "10Anomander Rake11Iskaral Pust12Dujek"]
                loadLog `shouldReturn` []

    describe "i18n" $ do
        it "opens the page" $ runWD $ do
            dir <- liftIO $ getCurrentDirectory
            openPage $ "file://" ++ dir ++ "/../client/" ++ filename

        it "displays the intl formatted data" $ runWD $ do
            "f-number" `intlSpanShouldBe` "90%"
            "f-int" `intlSpanShouldBe` "100,000"
            "f-double" `intlSpanShouldBe` "40,000.2"
            "f-shortday" `intlSpanShouldBe` "Jul 20, 1969"
            "f-fullday" `intlSpanShouldBe` "Sunday, July 20, 69 AD"
            "f-date" `intlSpanShouldBe` "Sun, Jul 20, 69"
            -- f-shorttime and f-fulltime cannot be (easily) tested since they rely on the current timezone
            "f-time" `intlSpanShouldBe` "Jul 19, 69, 4:56:00 PM GMT-10"
            "f-plural" `intlSpanShouldBe` "plural other"

            today <- liftIO (utctDay <$> getCurrentTime)
            let moon = fromGregorian 1969 7 20
                daysAgo = diffDays today moon
                yearsAgo :: Int = round $ realToFrac daysAgo / (365 :: Double) -- is close enough
            "f-relative" `intlSpanShouldBe` (show (yearsAgo) ++ " years ago")
            "f-relative-days" `intlSpanShouldBe` (showWithComma (daysAgo+1) ++ " days ago")

        it "displays messages" $ runWD $ do
            msg <- findElem $ ById "f-msg"
            getText msg `shouldReturn` "Neil Armstrong took 100 photos years ago."
            takenAgoSpan <- findElemFrom msg $ ById "takenAgoSpan"
            getText takenAgoSpan `shouldReturn` "years ago"

            msg' <- findElem $ ById "f-msg-with-descr"
            getText msg' `shouldReturn` "Neil Armstrong took no photos."

            "f-msg-with-trans" `intlSpanShouldBe` "message from translation xxx"

            htmlMsg <- findElem $ ById "f-html-msg"
            getText htmlMsg `shouldReturn` "42 is the answer to life, the universe, and everything"
            (findElemFrom htmlMsg (ByTag "b") >>= getText)
                `shouldReturn` "42"

            htmlMsg' <- findElem $ ById "f-html-msg-with-descr"
            getText htmlMsg' `shouldReturn` "42 is the answer to life, the universe, and everything"
            (findElemFrom htmlMsg' (ByTag "b") >>= getText)
                `shouldReturn` "answer"

        it "displays formatted properties" $ runWD $ do
            "f-number-prop" `intlPlaceholderShouldBe` "123,456"
            "f-date-prop" `intlPlaceholderShouldBe` "7/20/1969"
            "f-time-prop" `intlPlaceholderShouldBe` "Jul 19, 69, 4 PM"
            "f-plural-prop" `intlPlaceholderShouldBe` "other"
            "f-msg-prop" `intlPlaceholderShouldBe` "Neil Armstrong took 100 photos"
            "f-msg-prop-with-descr" `intlPlaceholderShouldBe` "Neil Armstrong took 0 photos"
