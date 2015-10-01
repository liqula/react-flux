-- | This module contains some useful combinators I have come across as I built a large
-- react-flux application.  None of these are required to use React.Flux, they just reduce somewhat
-- the typing needed to create rendering functions.
module React.Flux.Combinators (
    clbutton_
  , cldiv_
  , faIcon_
  , foreign_
  , labeledInput_
) where

import Data.Monoid ((<>))
import React.Flux.DOM
import React.Flux.Internal
import React.Flux.PropertiesAndEvents
import React.Flux.Views

#ifdef __GHCJS__
import GHCJS.Types (JSString, JSRef)

foreign import javascript unsafe
    "$r = window[$1]"
    js_lookupWindow :: JSString -> JSRef
#else
js_lookupWindow :: a -> ()
js_lookupWindow _ = ()
#endif

-- | A wrapper around 'foreignClass' that looks up the class on the `window`.  I use it for several
-- third-party react components.  For example, with <https://github.com/rackt/react-modal
-- react-modal>, assuming `window.Modal` contains the definition of the classes,
--
-- >foreign_ "Modal" [ "isOpen" @= isModelOpen myProps
-- >                 , callback "onRequestClose" $ dispatch closeModel
-- >                 , "style" @= Aeson.object [ "overlay" @= Aeson.object ["left" $= "50%", "right" $= "50%"]]
-- >                 ] $ do
-- >    h1_ "Hello, World!"
-- >    p_ "...."
--
-- Here is another example using <https://github.com/JedWatson/react-select react-select>:
--
-- >reactSelect_ :: [PropertyOrHandler eventHandler] -> ReactElementM eventHandler ()
-- >reactSelect_ props = foreign_ "Select" props mempty
-- >
-- >someView :: ReactView ()
-- >someView = defineView "some view" $ \() ->
-- >    reactSelect_
-- >        [ "name" $= "form-field-name"
-- >        , "value" $= "one"
-- >        , "options" @= [ object [ "value" .= "one", "label" .= "One" ]
-- >                       , object [ "value" .= "two", "label" .= "Two" ]
-- >                       ]
-- >        , callback "onChange" $ \(i :: String) -> dispatch $ ItemChangedTo i
-- >        ]
foreign_ :: String -- ^ this should be the name of a property on `window` which contains a react class.
         -> [PropertyOrHandler handler] -- ^ properties
         -> ReactElementM handler a -- ^ children
         -> ReactElementM handler a
foreign_ x = foreignClass (js_lookupWindow $ toJSString x)

-- | A 'div_' with the given class name (multiple classes can be separated by spaces).  This is
-- useful for defining rows and columns in your CSS framework of choice.  I use
-- <http://purecss.io/forms/ Pure CSS> so I use it something like:
--
-- >cldiv_ "pure-g" $ do
-- >    cldiv_ "pure-u-1-3" $ p_ "First Third"
-- >    cldiv_ "pure-u-1-3" $ p_ "Middle Third"
-- >    cldiv_ "pure-u-1-3" $ p_ "Last Third"
--
-- You should consider writing something like the following for the various components in your frontend
-- of choice.  In PureCSS, I use:
--
-- >prow_ :: ReactElementM handler a -> ReactElementM handler a
-- >prow_ = cldiv_ "pure-g"
-- >
-- >pcol_ :: String -> ReactElementM handler a -> ReactElementM handler a
-- >pcol_ cl = cldiv_ (unwords $ map ("pure-u-"++) $ words cl)
cldiv_ :: String -> ReactElementM handler a -> ReactElementM handler a
cldiv_ cl = div_ ["className" @= cl]

-- | A 'button_' with the given class names and `onClick` handler.
--
-- >clbutton_ ["pure-button button-success"] (dispatch LaunchMissiles) $ do
-- >    faIcon_ "rocket"
-- >    "Launch the missiles!"
clbutton_ :: String  -- ^ class names separated by spaces
          -> handler -- ^ the onClick handler for the button
          -> ReactElementM handler a -- ^ the children
          -> ReactElementM handler a
clbutton_ cl h = button_ ["className" @= cl, onClick (\_ _ -> h)]

-- | A 'label_' and an 'input_' together.  Useful for laying out forms.  For example, a
-- stacked <http://purecss.io/forms/ Pure CSS Form> could be
--
-- >form_ ["className" $= "pure-form pure-form-stacked"] $
-- >    fieldset_ $ do
-- >        legend_ "A stacked form"
-- >        labeledInput_ "email" "Email" ["type" $= "email"]
-- >        labeledInput_ "password"
-- >            ($(message "password-label" "Your password") [])
-- >            ["type" $= "password"]
--
-- The second 'labeledInput_' shows an example using "React.Flux.Addons.Intl".
labeledInput_ :: String -- ^ the ID for the input element
              -> ReactElementM handler () -- ^ the label content.  This is wrapped in a 'label_' with a `htmlFor` property
                                          -- equal to the given ID.
              -> [PropertyOrHandler handler] -- ^ the properties to pass to 'input_'.  A property with key `id` is added to this list of properties.
              -> ReactElementM handler ()
labeledInput_ ident lbl props = label_ ["htmlFor" @= ident] lbl <> input_ (("id" @= ident):props)

-- | A <http://fortawesome.github.io/Font-Awesome/ Font Awesome> icon.  The given string is prefixed
-- by `fa fa-` and then used as the class for an `i` element.  This allows you to icons such as
--
-- >faIcon_ "fighter-jet" -- produces <i class="fa fa-fighter-jet">
-- >faIcon_ "refresh fa-spin" -- produces <i class="fa fa-refresh fa-spin">
faIcon_ :: String -> ReactElementM handler ()
faIcon_ cl = i_ ["className" @= ("fa fa-" ++ cl)] mempty
