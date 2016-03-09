-- | This module contains some useful combinators I have come across as I built a large
-- react-flux application.  None of these are required to use React.Flux, they just reduce somewhat
-- the typing needed to create rendering functions.
{-# LANGUAGE DeriveAnyClass #-}
module React.Flux.Combinators (
    clbutton_
  , cldiv_
  , faIcon_
  , foreign_
  , labeledInput_
  -- * Ajax
  , initAjax
  , jsonAjax
  , AjaxRequest(..)
  , AjaxResponse(..)
  , ajax
) where

import Data.Monoid ((<>))
import React.Flux.DOM
import React.Flux.Internal
import React.Flux.PropertiesAndEvents
import React.Flux.Views
import Data.Text (Text)

#ifdef __GHCJS__
import GHCJS.Types (JSString, JSVal)
import GHCJS.Foreign.Callback
import GHCJS.Marshal (ToJSVal(..), toJSVal_aeson, FromJSVal(..))
import Data.Aeson (ToJSON, FromJSON, fromJSON, Result(..))
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Control.DeepSeq (deepseq)
import Control.Arrow ((***))
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.JSString as JSS
import qualified Data.JSString.Text as JSS

import React.Flux.Export
import React.Flux.Store (executeAction, SomeStoreAction)

foreign import javascript unsafe
    "$r = window[$1]"
    js_lookupWindow :: JSString -> JSVal
#else
js_lookupWindow :: a -> ()
js_lookupWindow _ = ()

type JSString = String
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
-- >clbutton_ ["pure-button button-success"] (dispatch LaunchTheMissiles) $ do
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

--------------------------------------------------------------------------------
--- Ajax
--------------------------------------------------------------------------------

-- | The input to an AJAX request built using @XMLHttpRequest@.
data AjaxRequest = AjaxRequest
  { reqMethod :: JSString
  , reqURI :: JSString
  , reqHeaders :: [(JSString, JSString)]
  , reqBody :: JSVal
  } deriving (Typeable, Generic, ToJSVal)

-- | The response after @XMLHttpRequest@ indicates that the @readyState@ is done.
data AjaxResponse = AjaxResponse
  { respStatus :: Int
  , respResponseText :: JSString
  , respResponseXHR :: JSVal -- ^ The raw @XMLHttpRequest@ object.  Use this if you want more status about the response,
                             --   such as response headers.
  } deriving (Typeable, Generic)

-- | GHCJS panics if we export the function directly, so wrap it in a data struct
data HandlerWrapper = HandlerWrapper { unwrapHandler :: AjaxResponse -> IO [SomeStoreAction] }
    deriving Typeable

-- | This is broken out as a separate function because if the code is inlined (either manually or by
-- ghc), there is a runtime error.
useHandler :: AjaxResponse -> HandlerWrapper -> IO ()
useHandler r (HandlerWrapper h) = do
    actions <- h r
    actions `deepseq` mapM_ executeAction actions
{-# NOINLINE useHandler #-} -- if this is inlined, there is a bug in ghcjs

-- | If you are going to use 'ajax' or 'jsonAjax', you must call 'initAjax' once from your main
-- function.  The call should appear before the call to 'reactRender'.
initAjax :: IO ()
#ifdef __GHCJS__
initAjax = do
    return ()
    a <- asyncCallback2 $ \rawXhr h -> do
        resp <- AjaxResponse <$> js_xhrStatus rawXhr <*> js_xhrResponseText rawXhr <*> pure rawXhr
        e <- js_getHandlerWrapper h
        js_release e
        parseExport e >>= useHandler resp

    js_setAjaxCallback a
#else
initAjax = return ()
#endif

-- | Use @XMLHttpRequest@ to send a request to the backend.  Once the response arrives
-- and the @readyState@ is done, the response will be passed to the given handler and the resulting
-- actions will be executed.  Note that 'ajax' returns immedietly and does not wait for the request
-- to finish.
ajax :: AjaxRequest -> (AjaxResponse -> IO [SomeStoreAction]) -> IO ()
#ifdef __GHCJS__
ajax req handler = do
    reqJson <- toJSVal req
    handlerE <- export $ HandlerWrapper handler
    js_ajax reqJson handlerE
#else
ajax _ _ = return ()
#endif

-- | Use @XMLHttpRequest@ to send a request with a JSON body, parse the response body as JSON, and
-- then dispatch some actions with the response.  This should be used from within the transform
-- function of your store.  For example,
--
-- >data Target = AlienShip | AlienPlanet
-- >  deriving (Show, Typeable, Generic, ToJSON, FromJSON)
-- >
-- >data Trajectory = Trajectory
-- >    { x :: Double, y :: Double, z :: Double, vx :: Double, vy :: Double, vz :: Double }
-- >  deriving (Show, Typeable, Generic, ToJSON, FromJSON)
-- >
-- >data UpdatePending = NoUpdatePending | UpdatePending Text | PreviousUpdateHadError Text
-- >
-- >data MyStore = MyStore { currentTrajectory :: Maybe Trajectory, launchUpdate :: UpdatePending }
-- >
-- >data MyStoreAction = LaunchTheMissiles Target
-- >                   | MissilesLaunched Trajectory
-- >                   | UnableToLaunchMissiles Text
-- >  deriving (Typeable, Generic, NFData)
-- >
-- >instance StoreData MyStore where
-- >    type StoreAction MyStore = MyStoreAction
-- >
-- >    transform (LaunchTheMissiles t) s = do
-- >        jsonAjax "PUT" "/launch-the-missiles" [] t $ \case ->
-- >            Left (_, msg) -> return [SomeStoreAction myStore $ UnableToLaunchMissiles msg]
-- >            Right traj -> return [SomeStoreAction myStore $ MissilesLaunched traj]
-- >        return s { launchUpdate = UpdatePending ("Requesting missle launch against " ++ T.pack (show t)) }
-- >
-- >    transform (MissilesLaunched traj) s =
-- >        return s { currentTrajectory = Just traj, launchUpdate = NoUpdatePending }
-- >
-- >    transform (UnableToLaunchMissiles err) s =
-- >        return s { launchUpdate = PreviousUpdateHadError err }
-- >
-- >myStore :: ReactStore MyStore
-- >myStore = mkStore $ MyStore Nothing NoUpdatePending
--
-- And then in your view, you can render this using something like:
-- 
-- >myView :: ReactView ()
-- >myView = defineControllerView "launch the missles" myStore $ \s () -> do
-- >    case launchUpdate s of
-- >        NoUpdatePending -> return ()
-- >        UpdatePending msg -> span_ $ faIcon_ "rocket" <> elemText (T.unpack msg)
-- >        PreviousUpdateHadErroer err -> span_ $ faIcon_ "exclamation" <> elemText (T.unpack err)
-- >    clbutton_ ["pure-button button-success"] ([SomeStoreAction myStore $ LaunchTheMissiles AlienShip]) $ do
-- >        faIcon_ "rocket"
-- >        "Launch the missles against the alien ship!"
-- >    p_ $ elemText $ "Current trajectory " ++ show (currentTrajectory s)
--
jsonAjax :: (ToJSON body, FromJSON response)
         => Text -- ^ the method
         -> Text -- ^ the URI
         -> [(Text, Text)] -- ^ the headers
         -> body -- ^ the body
         -> (Either (Int, Text) response -> IO [SomeStoreAction])
            -- ^ Once @XMLHttpRequest@ changes the @readyState@ to done this handler will be
            -- executed and the resulting actions dispatched to the stores.
            --
            --   * If the response status is @200@, the body will be parsed as JSON and a
            --     'Right' value will be passed to this handler.   If there is an
            --     error parsing the JSON response, a 'Left' value with @500@ and the error message
            --     from aeson is given to the handler. 
            --
            --   * If the response status is anything besides @200@, a 'Left' value with a pair
            --     of the response status and response text is passed to the handler.
         -> IO ()
#ifdef __GHCJS__
jsonAjax method uri headers body handler = do
    bodyRef <- toJSVal_aeson body
    let req = AjaxRequest
              { reqMethod = JSS.textToJSString method
              , reqURI = JSS.textToJSString uri
              , reqHeaders = map (JSS.textToJSString *** JSS.textToJSString) headers
              , reqBody = bodyRef
              }
    ajax req $ \resp ->
        if respStatus resp == 200
            then do
                json <- js_JSONParse $ respResponseText resp
                mv <- fromJSVal json
                case mv of
                    Nothing -> handler $ Left (500, "Unable to convert response body")
                    Just v -> case fromJSON v of
                        Success v' -> handler $ Right v'
                        Error e -> handler $ Left (500, T.pack e)
            else handler $ Left (respStatus resp, JSS.textFromJSString $ respResponseText resp)
#else
jsonAjax _ _ _ _ = return ()
#endif

#ifdef __GHCJS__
foreign import javascript unsafe
    "hsreact$ajax($1, $2)"
    js_ajax :: JSVal -> Export HandlerWrapper -> IO ()

foreign import javascript unsafe
    "hsreact$setAjaxCallback($1)"
    js_setAjaxCallback :: Callback (JSVal -> JSVal -> IO ()) -> IO ()

foreign import javascript unsafe
    "JSON['parse']($1)"
    js_JSONParse :: JSString -> IO JSVal

foreign import javascript unsafe
    "$1.hs"
    js_getHandlerWrapper :: JSVal -> IO (Export HandlerWrapper)

foreign import javascript unsafe
    "$1['status']"
    js_xhrStatus :: JSVal -> IO Int

foreign import javascript unsafe
    "$1['responseText']"
    js_xhrResponseText :: JSVal -> IO JSString

foreign import javascript unsafe
    "h$release($1)"
    js_release :: Export HandlerWrapper -> IO ()
#endif
