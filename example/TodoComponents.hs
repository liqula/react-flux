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

todoTextInput :: ReactView TextInputArgs
todoTextInput = defineStatefulView "todo text input" "" $ \curText args ->
    input_ $
        maybe [] (\i -> ["id" @= i]) (tiaId args)
        ++
        [ "className" @= tiaClass args
        , "placeholder" @= tiaPlaceholder args
        , "value" @= curText
        , "autoFocus" @= True
        , onBlur $ \_ _ curState -> ([tiaOnSave args curState | not $ null curState], Just "")
        , onChange $ \evt _ -> ([], Just $ target evt "value")
        , onKeyDown $ \_ evt curState ->
             if keyCode evt == 13 && not (null curState) -- 13 is enter
                 then ([tiaOnSave args curState], Just "")
                 else ([], Nothing)
        ]


todoTextInput_ :: TextInputArgs -> ReactElementM eventHandler ()
todoTextInput_ args = view todoTextInput args mempty
