{-# LANGUAGE OverloadedStrings #-}

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
todoTextInput = mkStatefulView "todo text input" "" $ \curText args ->
    input_ [ "className" @= tiaClass args
           , "placeholder" @= tiaPlaceholder args
           , "value" @= curText
           , "autoFocus" @= True
           , onBlur $ \_ _ curState -> ([tiaOnSave args curState], Nothing)
           , onChange $ \evt _ -> ([], Just $ target evt "value")
           , onKeyDown $ \_ evt curState -> ([ tiaOnSave args curState | keyCode evt == 13], Nothing) -- 13 is enter
           ] mempty

todoTextInput_ :: TextInputArgs -> ReactElementM eventHandler ()
todoTextInput_ args = rclass todoTextInput args mempty
