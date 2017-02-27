module Main (main) where

import React.Flux.Outdated
import App
import NavStore (initHistory)

main :: IO ()
main = do
    initHistory
    reactRender "side-menu-app" myApp ()
