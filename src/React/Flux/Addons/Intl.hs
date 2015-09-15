-- | Bindings to the <http://formatjs.io/react/ ReactIntl> library, which allows easy formatting of
-- numbers, dates, times, relative times, and pluralization which can be used even if you do not
-- intend to translate your application.  In addition, it provides a method for providing translations of
-- messages.
--
-- These bindings are currently against the 2.0 version of ReactIntl which is currently just a
-- pre-release.  For temporary documentation, see <https://github.com/yahoo/react-intl/issues/162 issue62>.
-- To use these bindings, you need to provide the @ReactIntl@ variable.  In the browser you can just
-- load the @react-intl.min.js@ script onto the page so that @window.ReactIntl@ exists.  If you are
-- running in node, execute @ReactIntl = require('ReactIntl');@ so that @global.ReactIntl@
-- exists.  When compiling with closure, protect the ReactIntl variable as follows:
--
-- >(function(global, React, ReactDOM, ReactIntl) {
-- >contents of all.js
-- >})(window, window['React'], window['ReactDOM'], window['ReactIntl]);
{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings #-}
module React.Flux.Addons.Intl(
    intlProvider

  -- * Numbers
  , int_
  , double_
  , formattedNumber_

  -- * Dates and Times
  , DayFormat(..)
  , shortDate
  , day_
  , TimeFormat(..)
  , shortDateTime
  , utcTime_
  , formattedDate_

  -- * Relative Times
  , relativeTo_
  , formattedRelative_

  -- * Plural

  -- * Messages
  , MessageId
  , Message(..)
  , message
  , message'
  , htmlMsg
  , htmlMsg'
  , writeIntlMessages
  , intlFormatJson
  , intlFormatJsonWithoutDescription
  , intlFormatAndroidXML
) where

import React.Flux
import Data.Time
import Control.Monad (when, forM_)
import Data.Monoid ((<>))
import Data.Maybe (catMaybes, fromMaybe)
import System.IO (withFile, IOMode(..))
import Language.Haskell.TH (runIO, Q, Loc, location, ExpQ)
import Language.Haskell.TH.Syntax (liftString, qGetQ, qPutQ, reportWarning, Dec)
import qualified Data.HashMap.Strict as H
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Aeson as Aeson

#ifdef __GHCJS__

import GHCJS.Types (JSRef)

foreign import javascript unsafe
    "$r = ReactIntl['IntlProvider']"
    js_intlProvider :: JSRef

foreign import javascript unsafe
    "$r = ReactIntl['FormattedNumber']"
    js_formatNumber :: JSRef

foreign import javascript unsafe
    "$r = ReactIntl['FormattedDate']"
    js_formatDate :: JSRef

foreign import javascript unsafe
    "$r = ReactIntl['FormattedRelative']"
    js_formatRelative :: JSRef

foreign import javascript unsafe
    "$r = ReactIntl['FormattedMessage']"
    js_formatMsg :: JSRef

foreign import javascript unsafe
    "$r = ReactIntl['FormattedHTMLMessage']"
    js_formatHtmlMsg :: JSRef

foreign import javascript unsafe
    "$r = (new Date($1, $2-1, $3))"
    js_mkDate :: Int -> Int -> Int -> JSRef

-- | Convert a day to a javascript Date
dayToRef :: Day -> JSRef
dayToRef day = js_mkDate (fromIntegral y) m d
    where
        (y, m, d) = toGregorian day

foreign import javascript unsafe
    "$r = (new Date(Date.UTC($1, $2-1, $3, $4, $5, $6, $7)))"
    js_mkDateTime :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> JSRef

-- | Convert a UTCTime to a javascript date object.
timeToRef :: UTCTime -> JSRef
timeToRef (UTCTime uday time) = js_mkDateTime (fromIntegral year) month day hour minute sec micro
    where
        (year, month, day) = toGregorian uday
        TimeOfDay hour minute pSec = timeToTimeOfDay time
        (sec, fracSec) = properFraction pSec
        micro = round $ fracSec * 1000000

#else

type JSRef = ()

js_intlProvider :: JSRef
js_intlProvider = ()

js_formatNumber :: JSRef
js_formatNumber = ()

js_formatDate :: JSRef
js_formatDate = ()

js_formatRelative :: JSRef
js_formatRelative = ()

js_formatMsg :: JSRef
js_formatMsg = ()

js_formatHtmlMsg :: JSRef
js_formatHtmlMsg = ()

dayToRef :: Day -> JSRef
dayToRef _ = ()

timeToRef :: UTCTime -> JSRef
timeToRef _ = ()

#endif

-- | Use the IntlProvider to set the @locale@ and @messages@ property.  @formats@ are not supported,
-- since it is easier to write Haskell wrappers around for example 'formattedNumber_' if you need
-- custom formats.
--
-- If you are going to use different locales, it is strongly recommended that you set the initial
-- locale from the server by reading the @Accept-Language@ header and/or a user setting so that the
-- page as a whole is consistent between the locale and the translated messages.  Therefore, I
-- recommend that in your server you dynamically create a small snippet such as
--
-- >window.myIntialConfig = { "locale": "en-US" };
--
-- and then in your react-flux app you can either just access this directly or load it into your
-- store if you allow the user to change the locale on the fly.
--
-- >foreign import javascript unsafe
-- >    "$r = window['myInitialConfig']['locale']"
-- >    js_initialLocale :: JSString
-- >
-- >myApp :: ReactView ()
-- >myApp = defineView "my application" $ \() ->
-- >    intlProvider_ (JSString.unpack js_initialLocale) Nothing $ ...
intlProvider_ :: String -- ^ the locale to use
              -> Maybe JSRef
                  -- ^ A reference to the translated messages. See below for more information on
                  -- providing translated messages.  Set this as Nothing if you are not using
                  -- translated messages.
              -> ReactElementM eventHandler a -- ^ the children of this element which will use the given locale and messages
              -> ReactElementM eventHandler a
intlProvider_ locale mmsgs = foreignClass js_intlProvider props
    where
        props = ("locale" @= locale) : [ property "messsages" msgs | let Just msgs = mmsgs ]

--------------------------------------------------------------------------------
--- Numbers
--------------------------------------------------------------------------------

-- | Format an integer using 'formattedNumber_' and the default style.
int_ :: Int -> ReactElementM eventHandler ()
int_ i = formattedNumber_ [ "value" @= i ]

-- | Format a double using 'formattedNumber_' and the default style.
double_ :: Double -> ReactElementM eventHandler ()
double_ d = formattedNumber_ [ "value" @= d ]

-- | A <http://formatjs.io/react/#formatted-number FormattedNumber> which allows arbitrary properties
-- and therefore allows control over the style and format of the number.  The accepted properties are
-- any options supported by
-- <https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/NumberFormat Intl.NumberFormat>.
formattedNumber_ :: [PropertyOrHandler eventHandler] -> ReactElementM eventHandler ()
formattedNumber_ props = foreignClass js_formatNumber props mempty

--------------------------------------------------------------------------------
-- Date/Time
--------------------------------------------------------------------------------

-- | How to display a date.  Each non-Nothing component will be displayed while the Nothing
-- components will be ommitted.  If everything is nothing, then it is assumed that year, month, and
-- day are each numeric.
--
-- These properties coorespond directly the options accepted by
-- <https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/DateTimeFormat Intl.DateTimeFormat>.
data DayFormat = DayFormat {
    weekdayF :: Maybe String -- ^ possible values are narrow, short, and long
  , eraF :: Maybe String -- ^ possible values are narrow, short, and long
  , yearF :: Maybe String -- ^ possible values are numeric and 2-digit
  , monthF :: Maybe String -- ^ possible values are numeric, 2-digit, narrow, short, and long
  , dayF :: Maybe String -- ^ possible values are numeric and 2-digit
} deriving Show

-- | Convert a format to the properties accepted by FormattedDate
dayFtoProps :: DayFormat -> [PropertyOrHandler handler]
dayFtoProps (DayFormat w e y m d) = catMaybes
    [ ("weekday"@=) <$> w
    , ("era"@=) <$> e
    , ("year"@=) <$> y
    , ("month"@=) <$> m
    , ("day"@=) <$> d
    ]

-- | A short day format, where month is \"short\" and year and day are \"numeric\".
shortDate :: DayFormat
shortDate = DayFormat
  { weekdayF = Nothing
  , eraF = Nothing
  , yearF = Just "numeric"
  , monthF = Just "short"
  , dayF = Just "numeric"
  }

-- | How to display a time.  Each non-Nothing component will be displayed while Nothing components
-- will be ommitted.
--
-- These properties coorespond directly the options accepted by
-- <https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/DateTimeFormat Intl.DateTimeFormat>.
data TimeFormat = TimeFormat {
    hourF :: Maybe String -- ^ possible values are numeric and 2-digit
  , minuteF :: Maybe String -- ^ possible values are numeric and 2-digit
  , secondF :: Maybe String -- ^ possible values are numeric and 2-digit
  , timeZoneNameF :: Maybe String -- ^ possible values are short and long
} deriving Show

-- | Convert a time format to properties for the FormattedDate element
timeFtoProps :: TimeFormat -> [PropertyOrHandler handler]
timeFtoProps (TimeFormat h m s t) = catMaybes
    [ ("hour"@=) <$> h
    , ("minute"@=) <$> m
    , ("second"@=) <$> s
    , ("timeZoneName"@=) <$> t
    ]

-- | A default date and time format, using 'shortDate' and then numeric for hour, minute, and
-- second.
shortDateTime :: (DayFormat, TimeFormat)
shortDateTime = (shortDate, TimeFormat
  { hourF = Just "numeric"
  , minuteF = Just "numeric"
  , secondF = Just "numeric"
  , timeZoneNameF = Nothing
  })

-- | Display a 'Day' in the given format using the @FormattedDate@ class and then wrap it in a
-- HTML5 <https://developer.mozilla.org/en-US/docs/Web/HTML/Element/time time> element.
day_ :: DayFormat -> Day -> ReactElementM eventHandler ()
day_ fmt day = time_ [property "dateTime" dateRef] $ foreignClass js_formatDate props mempty
    where
        dateRef = dayToRef day
        props = property "value" dateRef : dayFtoProps fmt

-- | Display a 'UTCTime' using the given format.  Despite giving the time in UTC, it will be
-- displayed to the user in their current timezone.  In addition, wrap it in a HTML5
-- <https://developer.mozilla.org/en-US/docs/Web/HTML/Element/time time> element.
utcTime_ :: (DayFormat, TimeFormat) -> UTCTime -> ReactElementM eventHandler ()
utcTime_ (dayFmt, timeF) t = time_ [property "dateTime" timeRef] $ foreignClass js_formatDate props mempty
    where
        timeRef = timeToRef t
        props = property "value" timeRef : (dayFtoProps dayFmt ++ timeFtoProps timeF)

-- | A raw <http://formatjs.io/react/#formatted-date FormattedDate> class which allows custom
-- properties to be passed.  The given 'Day' or 'UTCTime' will be converted to a javascript Date
-- object and passed in the @value@ property.  The remaining properties can be any properties that
-- <https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/DateTimeFormat Intl.DateTimeFormat>
-- accepts.  For example, you could pass in \"timeZone\" to specify a specific timezone to display.
formattedDate_ :: Either Day UTCTime -> [PropertyOrHandler eventHandler] -> ReactElementM eventHandler ()
formattedDate_ t props = foreignClass js_formatDate (valProp:props) mempty
    where
        valProp = property "value" $ either dayToRef timeToRef t


-- | Display the 'UTCTime' as a relative time.  In addition, wrap the display in a HTML5
-- <https://developer.mozilla.org/en-US/docs/Web/HTML/Element/time time> element.
relativeTo_ :: UTCTime -> ReactElementM eventHandler ()
relativeTo_ t = time_ [property "dateTime" timeRef] $ foreignClass js_formatRelative [property "value" timeRef] mempty
    where
        timeRef = timeToRef t

-- | Format the given UTCTime using the <http://formatjs.io/react/#formatted-relative FormattedRelative>
-- class to display a relative time to now.  The given 'UTCTime' is passed in the value property.
-- The supported style/formatting properties are \"units\" which can be one of second, minute, hour,
-- day, month, or year and \"style\" which if given must be numeric.
formattedRelative_ :: UTCTime -> [PropertyOrHandler eventHandler] -> ReactElementM eventHandler ()
formattedRelative_ t props = foreignClass js_formatRelative (property "value" (timeToRef t) : props) mempty

--------------------------------------------------------------------------------
-- Messages
--------------------------------------------------------------------------------

-- | An identifier for a message, must be unique.
type MessageId = T.Text

-- | A message
data Message = Message {
    msgDescription :: T.Text -- ^ A description intended to provide context for translators.
  , msgDefaultMsg :: T.Text -- ^ The default message written in ICU message syntax.
} deriving Show

-- | This is the type stored in the Q monad
type MessageMap = H.HashMap MessageId (Message, Loc)

-- | Utility function to build the properties for FormattedMessage.
messageToProps :: MessageId -> Message -> [PropertyOrHandler eventHandler] -> [PropertyOrHandler eventHandler]
messageToProps i (Message desc m) props = ["id" @= i, "description" @= desc, "defaultMessage" @= m, nestedProperty "values" props]

-- | Render a @FormattedMessage@ and also record it during compilation.  This template haskell
-- splice produces a value of type @[PropertyOrHandler eventHandler] -> ReactElementM eventHandler
-- ()@, which should be passed the values for the message (these properties are passed in the @values@
-- property of the @FormattedMessage@ class).  For example,
--
-- >li_ ["id" $= "some-id"] $
-- >    $(message "num_photos" "{name} took {numPhotos, plural, =0 {no photos} =1 {one photo} other {# photos}} {takenAgo}.")
-- >        [ "name" $= "Neil Armstrong"
-- >        , "numPhotos" @= (100 :: Int)
-- >        , elementProperty "takenAgo" $ relativeTo_ (UTCTime (fromGregorian 1969 7 20) (2*60*60 + 56*60))
-- >        ]
message :: MessageId
        -> T.Text -- ^ the default message written in ICU message syntax
        -> ExpQ --Q (TExp ([PropertyOrHandler eventHandler] -> ReactElementM eventHandler ()))
message ident m = formattedMessage [|js_formatMsg|] ident $ Message "" m

-- | Similar to 'message' but use a @FormattedHTMLMessage@ which allows HTML inside the message.  It
-- is recomended that you instead use 'message' together with 'elementProperty' to include rich text
-- inside the message property.  Again, this splice produces a value of type @[PropertyOrHandler
-- eventHandler] -> ReactElementM eventHandler ()@.
htmlMsg :: MessageId
        -> T.Text -- ^ default message written in ICU message syntax
        -> ExpQ
htmlMsg ident m = formattedMessage [|js_formatHtmlMsg|] ident $ Message "" m

-- | A variant of 'message' which allows you to specify some context for translators.
message' :: MessageId
         -> T.Text -- ^ A description indented to provide context for translators
         -> T.Text -- ^ The default message written in ICU message syntax
         -> ExpQ --Q (TExp ([PropertyOrHandler eventHandler] -> ReactElementM eventHandler ()))
message' ident descr m = formattedMessage [|js_formatMsg|] ident $ Message descr m

-- | A variant of 'htmlMsg' that allows you to specify some context for translators.
htmlMsg' :: MessageId
         -> T.Text -- ^ A description intended to provide context for translators
         -> T.Text -- ^ The default message written in ICU message syntax
         -> ExpQ
htmlMsg' ident descr m = formattedMessage [|js_formatHtmlMsg|] ident $ Message descr m

-- | Utility function for messages
formattedMessage :: ExpQ -> MessageId -> Message -> ExpQ --Q (TExp ([PropertyOrHandler eventHandler] -> ReactElementM eventHandler ()))
formattedMessage cls ident m = do
    curLoc <- location
    mmap :: MessageMap <- fromMaybe H.empty <$> qGetQ
    case H.lookup ident mmap of
        Just (prevMsg, prevLoc) | msgDefaultMsg m /= msgDefaultMsg prevMsg -> do
            reportWarning $ unlines
                [ "Message with id " ++ (T.unpack ident) ++ " appears twice with different messages"
                , show curLoc ++ ": " ++ (T.unpack $ msgDefaultMsg m)
                , show prevLoc ++ ": " ++ (T.unpack $ msgDefaultMsg prevMsg)
                ]
        _ -> return ()
    qPutQ $ H.insert ident (m, curLoc) mmap

    let liftText x = [| T.pack $(liftString $ T.unpack x)|]
        liftedMsg = [| Message $(liftText $ msgDescription m) $(liftText $ msgDefaultMsg m) |]
    [|\vals -> foreignClass $cls (messageToProps $(liftText ident) $liftedMsg vals) mempty |]

-- | Perform an arbitrary IO action on the accumulated messages at compile time, intended to write
-- the messages to a file.  Despite producing a value of type @Q [Dec]@, no declarations are
-- produced.  Instead, this is purly to allow IO to happen.  A call to this function should be
-- placed at the bottom of the file, since it only will output messages that appear above the call.
-- Also, to provide consistency, I suggest you create a utility wrapper around this function.  For
-- example,
--
-- >{-# LANGUAGE TemplateHaskell #-}
-- >module MessageUtil where
-- >
-- >import React.Flux.Addons.Intl
-- >
-- >writeMessages :: String -> Q [Dec]
-- >writeMessages name = writeIntlMessages (intlFormatJson $ "some/diretory/" ++ name ++ ".json")
--
-- Note that all paths in template haskell are relative to the directory containing the @.cabal@
-- file.  You can then use this as follows:
--
-- >{-# LANGUAGE TemplateHaskell #-}
-- >module SomeViews where
-- >
-- >import React.Flux
-- >import React.Flux.Addons.Intl
-- >import MessageUtil
-- >
-- >someView :: ReactView ()
-- >someView = defineView .... use $(message) in render ...
-- >
-- >anotherView :: ReactView ()
-- >anotherView = defineView ... use $(message) in render ...
-- >
-- >writeMessages "some-views"
--
-- Use this to produce one message file per Haskell view module.
writeIntlMessages :: (H.HashMap MessageId Message -> IO ()) -> Q [Dec]
writeIntlMessages f = do
    mmap :: MessageMap <- fromMaybe H.empty <$> qGetQ
    runIO $ f $ fmap fst mmap
    return []

-- | Format messages as json.  The format is an object where keys are the 'MessageId's, and the
-- value is an object with two properties, @message@ and optionally @description@.  This happens to
-- the the same format as <https://developer.chrome.com/extensions/i18n-messages chrome>.
intlFormatJson :: FilePath -> H.HashMap MessageId Message -> IO ()
intlFormatJson fp mmap = BL.writeFile fp $ Aeson.encode $ Aeson.Object $ fmap f mmap
    where
        f (Message "" m) = Aeson.object [(Aeson..=) "message" m]
        f (Message desc m) = Aeson.object [(Aeson..=) "message" m, (Aeson..=) "description" desc]

-- | Format messages as json, ignoring the description.  The format is an object where the keys are
-- the 'MessageId's and the value is the message string.  This format is used by many javascript
-- libraries, so many translation tools exist.
intlFormatJsonWithoutDescription :: FilePath -> H.HashMap MessageId Message -> IO ()
intlFormatJsonWithoutDescription fp mmap = BL.writeFile fp $ Aeson.encode $ Aeson.Object $ fmap f mmap
    where
        f (Message _ m) = Aeson.String m

-- | Format messages in <http://developer.android.com/guide/topics/resources/string-resource.html Android XML>
-- format.  There are many utilities to translate these XML messages, and the format has the
-- advantage that it can include the descriptions as well as the messages.
intlFormatAndroidXML :: FilePath -> H.HashMap MessageId Message -> IO ()
intlFormatAndroidXML fp mmap = withFile fp WriteMode $ \handle -> do
    B.hPut handle "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n"
    B.hPut handle "<resources>\n"
    let putText = B.hPut handle . T.encodeUtf8

    forM_ (H.toList mmap) $ \(ident, m) -> do
        when (msgDescription m /= "") $
            putText $ "<!-- " <> msgDescription m <> " -->\n"
        -- TODO: escape!!
        putText $ "<string name=\"" <> ident <> "\">" <> msgDefaultMsg m <> "</string>\n"
    B.hPut handle "</resources>\n"

{-




-- | Display a message using <http://formatjs.io/react/#formatted-message FormattedMessage>.  This
-- requires the @message@ property to be a string which contains the ICU Message syntax for the
-- message.  The class will cache the parsing of the string into the @intl-messageformat@ AST.
-- For formatted rich text objects, @FormattedMessage@ allows react elements to be passed as
-- properties and these properties can be created using 'elementProperty'.
--
-- The ReactIntl documentation shows how to use @this.getIntlMessage@ which looks up the string from
-- the @messages@ property passed to the mixin.  Instead, you should lookup the message string from
-- Haskell.  This allows the most flexibility in how the messages are loaded.
-- There are two approaches I suggest to managing messages.  First, you can use a 'ReactStore' to
-- manage the messages, for example writing the messages as JSON documents and loading them over
-- AJAX in response to the user changing the locale.  Then you can create a controller-view wrapper
-- around 'formattedMessage_' which perhaps takes a lens getter for the message.  Alternatively, if
-- you require a page reload to change the i18n (this is what I do), write the messages in raw
-- javascript which sets a variable on window.  For example, in a file @myMsgs.en-US.js@, have
--
-- >window.myMessages = window.myMessages || {};
-- >window.myMessages["en-US"] = {
-- >    photos: "{name} took {numPhotos, plural, =0 {no photos} =1 {one photo} other {# photos}} {takenAgo}."
-- >};
--
-- and similar files for each locale you support.  The server then includes these files depending on
-- the @Accept-Language@ header and/or user settings, and might always include @en-US@ for a
-- fallback.  Next, write accessor functions to access these messages from Haskell and write a
-- wrapper function around 'formattedMessage_' which looks up the message from the global message
-- object.
--
-- >foreign import javascript unsafe
-- >    "(window['myMessages'][window['myInitialConfig']['locale']][$1] || window['myMessages']['en-US'][$1])"
-- >    js_myMessage :: JSString -> JSRef
-- >
-- >message_ :: String -> [PropertyOrHandler eventHandler] -> ReactElementM eventHandler ()
-- >message_ m props = formattedMessage_ $ (property "message" $ js_myMessage m) : props
-- >
-- >someView :: ReactView ()
-- >someView = defineView "some view" $ \() ->
-- >    message_ "photos"
-- >       [ "name" $= "Neil Armstrong"
-- >       , "numPhotos" @= (100 :: Int)
-- >       , elementProperty "takenAgo" $ relativeTo_ (UTCTime (fromGregorian 1969 7 21) 0)
-- >       ]
formattedMessage_ :: [PropertyOrHandler eventHandler] -> ReactElementM eventHandler ()
formattedMessage_ props = foreignClass js_formatMsg props mempty

-- | Display a message using <http://formatjs.io/react/#formatted-html-message FormattedHTMLMessage>.
formattedHTMLMessage_ :: [PropertyOrHandler eventHandler] -> ReactElementM eventHandler ()
formattedHTMLMessage_ props = foreignClass js_formatHtmlMsg props mempty
-}
