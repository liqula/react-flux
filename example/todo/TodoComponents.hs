{-# LANGUAGE OverloadedStrings, BangPatterns, DataKinds #-}

-- | The division between a view and a component is arbitrary, but for me components are pieces that
-- are re-used many times for different purposes.  In the TODO app, there is one component for the
-- text box.
module TodoComponents where

import Data.Typeable (Typeable)
import React.Flux
import Data.JSString (JSString)
import qualified Data.Text as T

-- | The properties for the text input component.  Note how we can pass anything, including
-- functions, as the properties; the only requirement is an instance of Typeable.
data TextInputArgs = TextInputArgs {
      tiaId :: Maybe JSString
    , tiaClass :: JSString
    , tiaPlaceholder :: JSString
    , tiaOnSave :: T.Text -> [SomeStoreAction]
    , tiaValue :: Maybe T.Text
} deriving (Typeable)

-- | The text input stateful view.  The state is the text that has been typed into the textbox
-- but not yet saved.  The save is triggered either on enter or blur, which resets the state/content
-- of the text box to the empty string.
todoTextInput :: View '[TextInputArgs]
todoTextInput = mkStatefulView "todo text input" "" $ \curText args ->
    input_ $
        maybe [] (\i -> ["id" &= i]) (tiaId args)
        ++
        [ "className" &= tiaClass args
        , "placeholder" &= tiaPlaceholder args
        , "value" &= curText -- using value here creates a controlled component: https://facebook.github.io/react/docs/forms.html
        , "autoFocus" &= True

        -- Update the current state with the current text in the textbox, sending no actions
        , onChange $ \evt _ -> ([], Just $ target evt "value")

        -- Produce the save action and reset the current state to the empty string
        , onBlur $ \_ _ curState ->
            if not (T.null curState)
                then (tiaOnSave args curState, Just "")
                else ([], Nothing)
        , onKeyDown $ \_ evt curState ->
             if keyCode evt == 13 && not (T.null curState) -- 13 is enter
                 then (tiaOnSave args curState, Just "")
                 else ([], Nothing)
        ]

-- | A combinator suitible for use inside rendering functions.
todoTextInput_ :: TextInputArgs -> ReactElementM eventHandler ()
todoTextInput_ !args = view_ todoTextInput "todo-input" args
