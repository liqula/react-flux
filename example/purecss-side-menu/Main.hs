module Main (main) where

import React.Flux
import App
import NavStore (setOnPopState)

main :: IO ()
main = do
    setOnPopState
    reactRender "side-menu-app" myApp ()
