{-# LANGUAGE OverloadedStrings #-}
module TodoViews where

import Control.Monad (when)
import Data.List (intercalate)
import React.Flux

import Debug.Trace (trace)
import GHCJS.Types (JSRef)

import TodoStore
import TodoComponents

todoApp :: ReactView ()
todoApp = defineControllerView "todo app" todoStore $ \todoState () ->
    div_ $ do
        todoHeader_
        mainSection_ todoState
        todoFooter_ todoState
        testLifecycle_ (todoText $ snd $ head $ todoList todoState)

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
todoItem_ todo = viewWithKey todoItem (fst todo) todo mempty

todoFooter :: ReactView TodoState
todoFooter = defineView "footer" $ \(TodoState todos) ->
    let completed = length (filter (todoComplete . snd) todos)
        itemsLeft = length todos - completed
     in footer_ [ "id" $= "footer"] $ do

            span_ [ "id" $= "todo-count" ] $ do
                strong_ $ elemShow itemsLeft
                text $ if itemsLeft == 1 then "item left" else "items left"

            when (completed > 0) $ do
                button_ [ "id" $= "clear-completed"
                        , onClick $ \_ _ -> [todoA ClearCompletedTodos]
                        ] $
                    text $ "Clear completed (" ++ show completed ++ ")"

todoFooter_ :: TodoState -> ReactElementM eventHandler ()
todoFooter_ s = view todoFooter s mempty

logPandS :: LPropsAndState String Int -> IO ()
logPandS ps = do
    p <- lGetProps ps
    st <- lGetState ps
    trace ("Current props and state: " ++ p ++ ", " ++ show st) $ return ()

foreign import javascript
    "console.log($1)"
    js_logElem :: JSRef a -> IO ()

logDOM :: LDOM -> IO ()
logDOM dom = do
    lThis dom >>= js_logElem
    lRef dom "refSt" >>= js_logElem
    lRef dom "refProps" >>= js_logElem

testLifecycle :: ReactView String
testLifecycle = lifecycleView "testlifecycle" (12 :: Int) lifecycleConfig
    { lRender = \s p -> do
        span_ "Current state: "
        span_ ["ref" $= "refSt", "id" $= "hello"] (elemShow s)
        span_ ["ref" $= "refProps", "id" $= "world"] $ text $ "Current props: " ++ p
        button_ [ onClick $ \_ _ st -> ([], Just $ st + 1) ] "Incr"

    , lComponentWillMount = Just $ \pAndS setStateFn -> trace "will mount" $ do
        logPandS pAndS
        setStateFn 100

    , lComponentDidMount = Just $ \pAndS dom _setStateFn -> trace "did mount" $ do
        logPandS pAndS
        logDOM dom

    , lComponentWillReceiveProps = Just $ \pAndS dom _setStateFn newProps -> trace "will recv props" $ do
        logPandS pAndS
        logDOM dom
        trace ("New props: " ++ newProps) $ return ()

    , lComponentWillUpdate = Just $ \pAndS dom newProps newState -> trace "will update" $ do
        logPandS pAndS
        logDOM dom
        trace ("New props: " ++ newProps) $ trace ("New state: " ++ show newState) $ return ()

    , lComponentDidUpdate = Just $ \pAndS dom _setStateFn oldProps oldState -> trace "did update" $ do
        logPandS pAndS
        logDOM dom
        trace ("Old props: " ++ oldProps) $ trace ("Old state: " ++ show oldState) $ return ()

    , lComponentWillUnmount = Just $ \pAndS dom -> trace "will unmount" $ do
        logPandS pAndS
        logDOM dom
    }

testLifecycle_ :: String -> ReactElementM eventHandler ()
testLifecycle_ s = view testLifecycle s mempty
