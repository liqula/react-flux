{-# LANGUAGE OverloadedStrings #-}
module TodoViews where

import Control.Monad (when)
import Data.List (intercalate)
import React.Flux

import TodoStore
import TodoComponents

todoApp :: ReactView ()
todoApp = defineControllerView "todo app" todoStore $ \todoState () ->
    div_ $ do
        todoHeader_
        mainSection_ todoState
        todoFooter_ todoState

todoHeader :: ReactView ()
todoHeader = defineView "header" $ \() ->
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
           ]

    label_ [ "htmlFor" $= "toggle-all"] "Mark all as complete"
    ul_ [ "id" $= "todo-list" ] $ mapM_ todoItem_ $ todoList st

todoItem :: ReactView (Int, Todo)
todoItem = defineView "todo item" $ \(todoIdx, todo) ->
    li_ [ "className" @= (intercalate "," ([ "completed" | todoComplete todo] ++ [ "editing" | todoIsEditing todo ]) :: String)
        , "key" @= todoIdx
        ] $ do
        
        div_ [ "className" $= "view"] $ do
            input_ [ "className" $= "toggle"
                   , "type" $= "checkbox"
                   , "checked" @= todoComplete todo
                   , onChange $ \_ -> [todoA $ TodoSetComplete todoIdx $ not $ todoComplete todo]
                   ]

            label_ [ onDoubleClick $ \_ _ -> [todoA $ TodoEdit todoIdx] ] $
                text $ todoText todo

            button_ [ "className" $= "destroy"
                    , onClick $ \_ _ -> [todoA $ TodoDelete todoIdx]
                    ] mempty

        when (todoIsEditing todo) $
            todoTextInput_ TextInputArgs
                { tiaId = Nothing
                , tiaClass = "edit"
                , tiaPlaceholder = ""
                , tiaOnSave = todoA . UpdateText todoIdx
                , tiaValue = Just $ todoText todo
                }

todoItem_ :: (Int, Todo) -> ReactElementM eventHandler ()
todoItem_ todo = viewWithKey todoItem (fst todo) todo mempty

todoFooter :: ReactView TodoState
todoFooter = defineView "footer" $ \(TodoState todos) ->
    let completed = length (filter (todoComplete . snd) todos)
        itemsLeft = length todos - completed
     in footer_ [ "id" $= "footer"] $ do

            span_ [ "id" $= "todo-count" ] $ do
                strong_ $ elemShow itemsLeft
                text $ if itemsLeft == 1 then " item left" else " items left"

            when (completed > 0) $ do
                button_ [ "id" $= "clear-completed"
                        , onClick $ \_ _ -> [todoA ClearCompletedTodos]
                        ] $
                    text $ "Clear completed (" ++ show completed ++ ")"

todoFooter_ :: TodoState -> ReactElementM eventHandler ()
todoFooter_ s = view todoFooter s mempty
