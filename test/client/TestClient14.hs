{-# LANGUAGE OverloadedStrings, TypeFamilies, ScopedTypeVariables, TemplateHaskell #-}
module Main (main) where

import Data.Time
import React.Flux
import React.Flux.Addons.Intl

import TestClient

--------------------------------------------------------------------------------
--- Intl
--------------------------------------------------------------------------------

intlSpec :: ReactView ()
intlSpec = defineView "intl" $ \() -> div_ ["id" $= "intl-spec"] $ intlProvider_ "en-US" Nothing $
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

        {-
        li_ ["id" $= "f-relative"] $ relativeTo_ step
        li_ ["id" $= "f-relative-days"] $ formattedRelative_ step [ "units" $= "day" ]
        -}

        li_ ["id" $= "f-plural"] $ plural_ [ "value" @= (100 :: Int), "one" $= "plural one", "other" $= "plural other"]

        li_ ["id" $= "f-msg"] $
            $(message "photos" "{name} took {numPhotos, plural, =0 {no photos} =1 {one photo} other {# photos}} {takenAgo}.")
                [ "name" $= "Neil Armstrong"
                , "numPhotos" @= (100 :: Int)
                , elementProperty "takenAgo" $ span_ ["id" $= "takenAgoSpan"] "years ago"
                ]

        li_ ["id" $= "f-msg-with-descr"] $
            $(message' "photos2" "How many photos?" "{name} took {numPhotos, plural, =0 {no photos} =1 {one photo} other {# photos}}.")
                [ "name" $= "Neil Armstrong"
                , "numPhotos" @= (0 :: Int)
                ]

        {-
        li_ ["id" $= "f-html-msg"] $
            formattedHTMLMessage_
                [ "message" $= "<b>{num}</b> is the answer to life, the universe, and everything"
                , "num" @= (42 :: Int)
                ]
        -}

--------------------------------------------------------------------------------
--- Main
--------------------------------------------------------------------------------

-- | Test a lifecycle view with all lifecycle methods nothing
app :: ReactView ()
app = defineView "main app" $ \() -> do
    view testClient () mempty
    view intlSpec () mempty

main :: IO ()
main = reactRender "app" app ()

writeIntlMessages (intlFormatJson "test/.jsonmsgs.json")
writeIntlMessages (intlFormatJsonWithoutDescription "test/.jsonnodescr.json")
writeIntlMessages (intlFormatAndroidXML "test/.android.xml")
