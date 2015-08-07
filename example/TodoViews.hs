{-# LANGUAGE OverloadedStrings #-}
module TodoViews where

import Control.Monad (when)
import Data.List (intercalate)
import Data.String (IsString(..))
import React.Flux

import TodoStore
import TodoComponents

todoApp :: ReactView ()
todoApp = mkControllerView "todo app" todoStore $ \todoState () ->
    div_ $ do
        todoHeader_
        mainSection_ todoState
        --todoFooter_ todoState

todoHeader :: ReactView ()
todoHeader = mkView "header" $ \() ->
    header_ ["id" $= "header"] $ do
        h1_ "todos"
        todoTextInput_  TextInputArgs
          { tiaId = Just "new-todo"
          , tiaClass = "new-todo"
          , tiaPlaceholder = "What needs to be done?"
          , tiaOnSave = todoA . TodoCreate
          , tiaValue = Nothing
          }

todoHeader_ :: ReactElementM eventHandler ()
todoHeader_ = view todoHeader () mempty

mainSection_ :: TodoState -> ReactElementM ViewEventHandler ()
mainSection_ st = section_ ["id" $= "main"] $ do
    input_ [ "id" $= "toggle-all"
           , "type" $= "checkbox"
           , "checked" $= if all (todoComplete . snd) $ todoList st then "checked" else ""
           , onChange $ \_ -> [todoA ToggleAllComplete]
           ] mempty

    label_ [ "htmlFor" $= "toggle-all"] "Mark all as complete"
    ul_ [ "id" $= "todo-list" ] $ mapM_ todoItem_ $ todoList st

todoItem :: ReactView (Int, Todo)
todoItem = mkView "todo item" $ \(todoIdx, todo) ->
    li_ [ "className" @= (intercalate "," ([ "completed" | todoComplete todo] ++ [ "editing" | todoIsEditing todo ]) :: String)
        , "key" @= todoIdx
        ] $ do
        
        div_ [ "className" $= "view"] $ do
            input_ [ "className" $= "toggle"
                   , "type" $= "checkbox"
                   , "checked" @= todoComplete todo
                   , onChange $ \_ -> [todoA $ TodoSetComplete todoIdx $ not $ todoComplete todo]
                   ] mempty

            label_ [ onDoubleClick $ \_ _ -> [todoA $ TodoEdit todoIdx] ] $
                fromString $ todoText todo

            button_ [ "className" $= "destroy"
                    , onClick $ \_ _ -> [todoA $ TodoDelete todoIdx]
                    ]
                    "Delete"

        when (todoIsEditing todo) $
            todoTextInput_ TextInputArgs
                { tiaId = Nothing
                , tiaClass = "edit"
                , tiaPlaceholder = ""
                , tiaOnSave = todoA . UpdateText todoIdx
                , tiaValue = Just $ todoText todo
                }

todoItem_ :: (Int, Todo) -> ReactElementM eventHandler ()
todoItem_ (idx, todo) = viewWithKey todoItem idx (idx, todo) mempty
