-- | Bindings to the <http://formatjs.io/react/ ReactIntl> library.
--
-- To use these bindings, load the @react-intl.min.js@ script onto the page so that
-- @window.ReactIntl@ exists.  Next, add a call to 'setLocales_' to the top level of your
-- application and then use the various formatting combinators in your rendering functions.
--
-- Note that the mixin is only used for the locale, the formats and messages are better managed from
-- Haskell.  The @formats@ property of the mixin allows you to specify custom number and date
-- formats so as to shorten the resulting properties you need to pass to @FormattedNumber@ and
-- friends, but it is easier to just create a Haskell utility function wrapping for example
-- 'foramttedNumber_' if you need custom formats.  Finally, the @messages@ passed down through the
-- mixin is used only for the @getIntlMessage()@ function, and it is better to just load the
-- messages from Haskell (see 'formattedMessage_' for more details).
module React.Flux.Addons.Intl(
    setLocales_
  , formattedNumber_
  , int_
  , double_
  , formattedDate_
  , day_
  , formattedTime_
  , utcTime_
  , formattedRelative_
  , relativeTo_
  , formattedMessage_
  , formattedHTMLMessage_
) where

import React.Flux
import Data.Time
import GHCJS.Types (JSRef)

foreign import javascript unsafe
    "hsreact$intl_mixin_class()"
    js_intlMixinClass :: JSRef

foreign import javascript unsafe
    "window['ReactIntl']['FormatNumber']"
    js_formatNumber :: JSRef

foreign import javascript unsafe
    "window['ReactIntl']['FormattedDate']"
    js_formatDate :: JSRef

foreign import javascript unsafe
    "window['ReactIntl']['FormattedTime']"
    js_formatTime :: JSRef

foreign import javascript unsafe
    "window['ReactIntl']['FormattedRelative']"
    js_formatRelative :: JSRef

foreign import javascript unsafe
    "window['ReactIntl']['FormattedMessage']"
    js_formatMsg :: JSRef

foreign import javascript unsafe
    "window['ReactIntl']['FormattedHTMLMessage']"
    js_formatHtmlMsg :: JSRef

foreign import javascript unsafe
    "(new Date($1, $2, $3))"
    js_mkDate :: Int -> Int -> Int -> JSRef

foreign import javascript unsafe
    "(new Date(Date.UTC($1, $2, $3, $4, $5, $6, $7)))"
    js_mkDateTime :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> JSRef

-- | Use the IntlMixin to set the @locales@ property.  This @locales@ property will be passed to all
-- nested formatters.  It is strongly recommended that you set the initial locale from the server by
-- reading the @Accept-Language@ header and/or a user setting so that the page as a whole is
-- consistent between the locale and the translated messages.  Therefore, I recommend that in your
-- server you dynamically create a small snippet such as
--
-- >window.myIntialConfig = { "locale": "en-US" };
--
-- and then in your react-flux app you can either just access this directly or load it into your
-- store if you allow the user to change the locale on the fly.
--
-- >foreign import javascript unsafe
-- >    "window['myInitialConfig']['locale']"
-- >    js_initialLocale :: JSString
-- >
-- >myApp :: ReactView ()
-- >myApp = defineView "my application" $ \() ->
-- >    setLocales_ [JSString.unpack js_initialLocale] $ ...
setLocales_ :: [String] -> ReactElementM eventHandler a -> ReactElementM eventHandler a
setLocales_ locales = foreignClass js_intlMixinClass [ "locales" @= locales ]

-- | A <http://formatjs.io/react/#formatted-number FormattedNumber> which allows arbitrary properties
-- and therefore allows control over the style and format of the number.  Use 'int_' or 'double_' if
-- you are OK with the default style.
formattedNumber_ :: [PropertyOrHandler eventHandler] -> ReactElementM eventHandler ()
formattedNumber_ props = foreignClass js_formatNumber props mempty

-- | Format an integer using 'formattedNumber_' and the default style.
int_ :: Int -> ReactElementM eventHandler ()
int_ i = formattedNumber_ [ "value" @= i ]

-- | Format a double using 'formattedNumber_' and the default style.
double_ :: Double -> ReactElementM eventHandler ()
double_ d = formattedNumber_ [ "value" @= d ]

-- | Format a day using the <http://formatjs.io/react/#formatted-date FormattedDate> class using
-- custom styles and/or formats.  The given 'Day' will be converted to a javascript Date object
-- and passed in the @value@ property.  Thus the list of properties should just give the style
-- and format.
formattedDate_ :: Day -> [PropertyOrHandler eventHandler] -> ReactElementM eventHandler ()
formattedDate_ day props = foreignClass js_formatDate (valProp:props) mempty
    where
        (y, m, d) = toGregorian day
        valProp = property "value" $ js_mkDate (fromIntegral y) m d

-- | Format a day using the @FormattedDate@ class with the default style and then wrapped in a
-- HTML5 <https://developer.mozilla.org/en-US/docs/Web/HTML/Element/time time> element.
day_ :: Day -> ReactElementM eventHandler ()
day_ day = time_ [property "dateTime" dateRef] $ foreignClass js_formatDate [property "value" dateRef] mempty
    where
        (y, m, d) = toGregorian day
        dateRef = js_mkDate (fromIntegral y) m d

-- | Convert a UTCTime to a javascript date object.
timeToRef :: UTCTime -> JSRef
timeToRef (UTCTime uday time) = js_mkDateTime (fromIntegral year) month day hour minute sec micro
    where
        (year, month, day) = toGregorian uday
        TimeOfDay hour minute pSec = timeToTimeOfDay time
        (sec, fracSec) = properFraction pSec
        micro = round $ fracSec * 1000000

-- | Format the given UTCTime using the <http://formatjs.io/react/#formatted-time FormattedTime> class.
-- Note that even though you specify the time in UTC, it will be displayed to the user in the local
-- time zone using the current locale.  To do so, the UTCTime is converted to a javascript Date
-- object (which are always UTC) and passed as the @value@ property.  Therefore, the list of properties
-- should just give the style and format.
formattedTime_ :: UTCTime -> [PropertyOrHandler eventHandler] -> ReactElementM eventHandler ()
formattedTime_ t props = foreignClass js_formatTime (property "value" (timeToRef t) : props) mempty

-- | Display a UTCTime to the user in their current timezone and locale.  In addition, wrap the
-- display in a HTML5 <https://developer.mozilla.org/en-US/docs/Web/HTML/Element/time time> element.
utcTime_ :: UTCTime -> ReactElementM eventHandler ()
utcTime_ t = time_ [property "dateTime" timeRef] $ foreignClass js_formatTime [property "value" timeRef] mempty
    where
        timeRef = timeToRef t

-- | Format the given UTCTime using the <http://formatjs.io/react/#formatted-relative FormattedRelative>
-- class to display a relative time to now.  Again, the list of properties should just
-- give the style and format, since the UTCTime is passed in the @value@ property.
formattedRelative_ :: UTCTime -> [PropertyOrHandler eventHandler] -> ReactElementM eventHandler ()
formattedRelative_ t props = foreignClass js_formatRelative (property "value" (timeToRef t) : props) mempty

-- | Display the given UTCTime as a relative time.  In addition, wrap the display in a HTML5
-- <https://developer.mozilla.org/en-US/docs/Web/HTML/Element/time time> element.
relativeTo_ :: UTCTime -> ReactElementM eventHandler ()
relativeTo_ t = time_ [property "dateTime" timeRef] $ foreignClass js_formatRelative [property "value" timeRef] mempty
    where
        timeRef = timeToRef t

-- | Display a message using <http://formatjs.io/react/#formatted-message FormattedMessage>.  This
-- requires the @message@ property to be a string which contains the ICU Message syntax for the
-- message.  The class will cache the parsing of the string into the @intl-messageformat@ AST.
-- For formatted rich text objects, @FormattedMessage@ allows react elements to be passed as
-- properties.  These properties can be created using 'elementProperty'.
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
-- >    photos: "{name} took {numPhotos, plural, =0 {no photos}, =1 {one photo}, other {# photos}} {takenAgo}."
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
-- >message_ msg props = formattedMessage_ $ (property "message" $ js_myMessage msg) : props
-- >
-- >someView :: ReactView ()
-- >someView = defineView "some view" $ \() ->
-- >    message_ "photos"
-- >       [ "name" $= "Neil Armstrong"
-- >       , "numPhotos" @= (100 :: Int)
-- >       , elementProperty "takenAgo" $ relativeTo_ (UTCTime (fromGregorian 1969 7 21) 0)
-- >       ]
--
-- Note that since there is no IO on the @js_myMessage@ type, you cannot change the contents of the
-- message dictionary at runtime!
formattedMessage_ :: [PropertyOrHandler eventHandler] -> ReactElementM eventHandler ()
formattedMessage_ props = foreignClass js_formatMsg props mempty

-- | Display a message using <http://formatjs.io/react/#formatted-html-message FormattedHTMLMessage>.
formattedHTMLMessage_ :: [PropertyOrHandler eventHandler] -> ReactElementM eventHandler ()
formattedHTMLMessage_ props = foreignClass js_formatHtmlMsg props mempty
