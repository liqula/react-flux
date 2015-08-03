module TodoComponents where

import Data.Typeable (Typeable)
import React.Flux

data TextInputArgs = TextInputArgs {
      tiaId :: Maybe String
    , tiaClass :: String
    , tiaPlaceholder :: String
    , tiaOnSave :: String -> SomeStoreAction
    , tiaValue :: Maybe String
} deriving (Typeable)

todoTextInput :: ReactClass TextInputArgs
todoTextInput = mkStatefulView "" $ \curText args ->
    input_ [ "className" @= tiaClass args
           , "placeholder" @= tiaPlaceholder args
           , "value" @= curText
           , "autoFocus" @= True
           , onBlur $ \_ _ curState -> ([tiaOnSave curState], Nothing)
           , onChange $ \_ evt _ -> ([], Just evtValue evt)
           , onKeyDown $ \_ evt curState -> ([ tiaOnSave curState | evtCode evt == 13], Nothing) -- 13 is enter
           ]
